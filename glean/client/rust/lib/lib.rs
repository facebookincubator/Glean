use async_trait::async_trait;
use build_info::BuildInfo;
use byteorder::BigEndian;
use byteorder::ByteOrder;
use configerator_client::ConfigeratorError;
use configerator_client::{cpp_sync_client::ConfigeratorCppClient, ConfigeratorClientExt};
use fbinit::FacebookInit;
use fbthrift::compact_protocol::CompactProtocolDeserializer;
use fbthrift::Deserialize;
use fbthrift::NonthrowingFunctionError;
use glean::client::{make_GleanService, GleanService};
use glean::glean_service::UserQueryError;
use glean::glean_service::UserQueryFactsError;
use glean::types::*;
use glean_client_config::ClientConfig;
use glean_client_config::UseShards;
use glean_service::Service;
use maplit::hashmap;
use md5::{Digest, Md5};
use srclient::SRChannelBuilder;
use std::collections::HashMap;
use std::net::AddrParseError;
use std::net::SocketAddr;
use std::sync::Arc;
use std::time::Duration;
use std::time::SystemTimeError;
use std::time::{SystemTime, UNIX_EPOCH};
use thiserror::Error;
use thriftclient::ThriftChannelBuilder;
use tokio::sync::AcquireError;
use tokio::sync::Semaphore;
use tracing::{debug, warn};
use user::current_username;

#[derive(Error, Debug, Clone)]
pub enum GleanClientError {
    #[error("Unsupported results encoding. Only compact supported")]
    UnsupportedResultsEncoding,
    #[error("Couldn't find latest repo for `{0}` and repo_hash is missing")]
    DatabaseNotAvailable(String),
    #[error("Fact not found")]
    FactNotFound,
    #[error("Unsupported service configuration")]
    UnsupportedServiceConfiguration,
    #[error("Internal error: {0}")]
    InternalError(String),
    #[error("Query error: {0}")]
    QueryError(String),
    #[error("thrift error: {0}")]
    ThriftFunctionError(String),
}

#[async_trait]
pub trait GleanClient {
    async fn get_all<T>(
        &self,
        recursive: bool,
        max_results: Option<i64>,
    ) -> Result<Vec<T>, GleanClientError>
    where
        for<'a> T: Deserialize<CompactProtocolDeserializer<std::io::Cursor<&'a [u8]>>>;

    async fn get_by_id<T>(&self, id: i64) -> Result<T, GleanClientError>
    where
        for<'a> T: Deserialize<CompactProtocolDeserializer<std::io::Cursor<&'a [u8]>>>;

    async fn get_multiple_by_id<T>(&self, ids: Vec<i64>) -> Result<Vec<T>, GleanClientError>
    where
        for<'a> T: Deserialize<CompactProtocolDeserializer<std::io::Cursor<&'a [u8]>>>;

    async fn run_angle_query<T>(
        &self,
        query_string: String,
        recursive: bool,
        max_results: Option<i64>,
    ) -> Result<Vec<T>, GleanClientError>
    where
        for<'a> T: Deserialize<CompactProtocolDeserializer<std::io::Cursor<&'a [u8]>>>;

    async fn run_single_angle_query(
        &self,
        query: String,
        recursive: bool,
        max_results: Option<i64>,
        continuation: Option<UserQueryCont>,
    ) -> Result<(Vec<Vec<u8>>, Option<UserQueryCont>), GleanClientError>;
}

pub struct Glean {
    client: Arc<dyn GleanService + Send + Sync>,
    repo: Repo,
    semaphore: Arc<Semaphore>,
    client_info: UserQueryClientInfo,
}

const MAX_CONCURRENT_QUERIES: usize = 100;
const QUERY_MAX_RESULTS: i64 = 200;

pub struct GleanConfig {
    fb: FacebookInit,
    repo_name: String,
    repo_hash: Option<String>,
    client_config: Option<ClientConfig>,
}

impl GleanConfig {
    pub fn new(fb: FacebookInit, repo_name: String) -> GleanConfig {
        GleanConfig {
            fb,
            repo_name,
            repo_hash: None,
            client_config: None,
        }
    }

    pub fn with_repo_hash(mut self, hash: String) -> GleanConfig {
        self.repo_hash = Some(hash);
        self
    }

    pub fn with_client_config(mut self, client_config: ClientConfig) -> GleanConfig {
        self.client_config = Some(client_config);
        self
    }

    pub async fn to_client(self) -> Result<Glean, GleanClientError> {
        let client_config = match self.client_config {
            Some(config) => config,
            None => fetch_config(self.fb)?,
        };
        let repo = match self.repo_hash {
            Some(hash) => Repo {
                name: self.repo_name,
                hash,
            },
            None => {
                let temp_client =
                    Glean::create_glean_service(self.fb, &client_config, None).await?;
                let latest =
                    Glean::get_latest_repo(self.fb, temp_client, &self.repo_name, &client_config)
                        .await?
                        .ok_or(GleanClientError::DatabaseNotAvailable(self.repo_name))?;
                debug!("Use latest database: {}/{}", latest.name, latest.hash);
                latest
            }
        };
        Glean::new(self.fb, repo, client_config).await
    }
}

fn fetch_config(fb: FacebookInit) -> Result<ClientConfig, GleanClientError> {
    let cfgr = ConfigeratorCppClient::new(fb)?;
    let config_name = "glean/client/client";
    let timeout = Duration::from_millis(100);
    let config: ClientConfig = cfgr.get_parsed_config(config_name, Some(timeout))?;
    Ok(config)
}

impl Glean {
    async fn get_latest_repo<'a>(
        fb: FacebookInit,
        client: Arc<dyn GleanService + Send + Sync>,
        repo_name: &'a str,
        config: &'a ClientConfig,
    ) -> Result<Option<Repo>, GleanClientError> {
        let query = ListDatabases::default();
        let result: ListDatabasesResult = client.listDatabases(&query).await?;
        let dbs: Vec<Database> = result.databases;
        let now = SystemTime::now();
        let since_the_epoch = now.duration_since(UNIX_EPOCH)?;
        let now_secs = since_the_epoch.as_secs();
        Ok(dbs
            .into_iter()
            .filter(|db| db.repo.name == repo_name)
            .filter(|db| db.created_since_epoch.is_some())
            .filter(|db| {
                now_secs - (db.created_since_epoch.unwrap() as u64) > config.min_db_age as u64
            })
            .filter(|db| Glean::db_available(fb, db, config))
            .filter(|db| db.expire_time.is_none())
            .max_by_key(|db| db.created_since_epoch.unwrap())
            .map(|db| db.repo))
    }

    fn db_available(fb: FacebookInit, db: &Database, config: &ClientConfig) -> bool {
        match (db.status, Glean::use_shard(config)) {
            (Some(DatabaseStatus::Complete), _) => true,
            (Some(DatabaseStatus::Restoring), true) => Glean::sr_has_shard(fb, db, config)
                .unwrap_or_else(|e| {
                    warn!("ServiceRouter request failed: {}", e);
                    false
                }),
            _ => false,
        }
    }

    fn sr_has_shard(
        fb: FacebookInit,
        db: &Database,
        client_config: &ClientConfig,
    ) -> Result<bool, GleanClientError> {
        match &client_config.serv {
            Service::tier(tier) => {
                let service_options = Glean::create_service_options(client_config, Some(&db.repo));
                let conn_config = Glean::create_conn_config(client_config);
                let hosts = SRChannelBuilder::from_service_name(fb, &tier)?
                    .with_service_options(&service_options)
                    .with_conn_config(&conn_config)
                    .get_selection()?;
                debug!(
                    "{}/{}: this db is available on {} other servers ({:?})",
                    db.repo.name,
                    db.repo.hash,
                    hosts.len(),
                    hosts
                );
                Ok(!hosts.is_empty())
            }
            _ => Ok(false),
        }
    }

    fn use_shard(client_config: &ClientConfig) -> bool {
        client_config.use_shards != UseShards::NO_SHARDS
    }

    fn create_conn_config(client_config: &ClientConfig) -> HashMap<String, String> {
        hashmap! {
            "client_id".to_string() => BuildInfo::get_rule().to_string(),
            "overall_timeout".to_string() => format!("{}", client_config.host_timeout_ms),
        }
    }

    fn create_service_options(
        client_config: &ClientConfig,
        repo: Option<&Repo>,
    ) -> HashMap<String, Vec<String>> {
        let mut service_options: HashMap<String, Vec<String>> = HashMap::new();
        if Glean::use_shard(client_config) {
            if let Some(a) = repo {
                let repo_str = format!("{}/{}", a.name, a.hash);
                let mut hasher = Md5::new();
                hasher.input(repo_str);
                let hash = hasher.result();
                let buf = &hash[0..8];
                let shard = BigEndian::read_u64(&buf) >> 1;
                service_options.insert("shards".to_string(), vec![format!("{}", shard)]);
            }
        }
        service_options
    }

    async fn create_glean_service(
        fb: FacebookInit,
        client_config: &ClientConfig,
        repo: Option<&Repo>,
    ) -> Result<Arc<dyn GleanService + Send + Sync>, GleanClientError> {
        match &client_config.serv {
            Service::tier(tier) => {
                let service_options = Glean::create_service_options(client_config, repo);
                let conn_config = Glean::create_conn_config(client_config);
                let ch = SRChannelBuilder::from_service_name(fb, &tier)?
                    .with_service_options(&service_options)
                    .with_conn_config(&conn_config);
                Ok(ch.build_client(make_GleanService)?)
            }
            Service::hostPort(host_port) => {
                let host = &host_port.host;
                let port = host_port.port;
                let socket = SocketAddr::new(host.parse()?, port as u16);
                let ch = ThriftChannelBuilder::from_sock_addr(fb, socket)
                    .with_conn_timeout(client_config.host_timeout_ms as u32)
                    .with_recv_timeout(client_config.host_timeout_ms as u32);
                Ok(ch.build_client(make_GleanService)?)
            }
            _ => Err(GleanClientError::UnsupportedServiceConfiguration {}),
        }
    }

    pub async fn new(
        fb: FacebookInit,
        repo: Repo,
        client_config: ClientConfig,
    ) -> Result<Self, GleanClientError> {
        let client = Glean::create_glean_service(fb, &client_config, Some(&repo)).await?;
        Ok(Glean {
            client,
            repo,
            semaphore: Arc::new(Semaphore::new(MAX_CONCURRENT_QUERIES)),
            client_info: UserQueryClientInfo {
                name: "api-rust".to_string(),
                unixname: current_username().ok(),
                application: BuildInfo::get_rule().to_string(),
            },
        })
    }
    const GLEAN_PREFIX: &'static str = "glean_schema_";

    fn glean_name<T>() -> String {
        return std::any::type_name::<T>()
            .to_string()
            .replace(Glean::GLEAN_PREFIX, "")
            .replace("::", ".")
            .replace(".types", "");
    }
}

#[async_trait]
impl GleanClient for Glean {
    async fn get_all<T>(
        &self,
        recursive: bool,
        max_results: Option<i64>,
    ) -> Result<Vec<T>, GleanClientError>
    where
        for<'a> T: Deserialize<CompactProtocolDeserializer<std::io::Cursor<&'a [u8]>>>,
    {
        let query = format!("{} _", Glean::glean_name::<T>());
        self.run_angle_query(query, recursive, max_results).await
    }

    async fn get_by_id<T>(&self, id: i64) -> Result<T, GleanClientError>
    where
        for<'a> T: Deserialize<CompactProtocolDeserializer<std::io::Cursor<&'a [u8]>>>,
    {
        let result = self
            .get_multiple_by_id(vec![id])
            .await?
            .into_iter()
            .next()
            .ok_or(GleanClientError::FactNotFound {})?;
        Ok(result)
    }

    async fn get_multiple_by_id<T>(&self, ids: Vec<i64>) -> Result<Vec<T>, GleanClientError>
    where
        for<'a> T: Deserialize<CompactProtocolDeserializer<std::io::Cursor<&'a [u8]>>>,
    {
        let query_params: Vec<FactQuery> = ids
            .into_iter()
            .map(|id| FactQuery {
                id,
                predicate_version: Option::None,
                recursive: false,
            })
            .collect();
        let encodings = vec![UserQueryEncoding::compact(
            UserQueryEncodingCompact::default(),
        )];
        let query_facts = UserQueryFacts {
            facts: query_params,
            encodings,
            client_info: Some(self.client_info.clone()),
            ..Default::default()
        };
        let permit = self.semaphore.acquire().await?;
        let resp = self.client.userQueryFacts(&self.repo, &query_facts).await?;
        drop(permit);
        let results: UserQueryEncodedResults = resp.results;
        match results {
            UserQueryEncodedResults::compact(compact_results) => Ok(compact_results
                .facts
                .iter()
                .map(fbthrift::compact_protocol::deserialize)
                .collect::<Result<Vec<_>, _>>()?),
            _ => Err(GleanClientError::UnsupportedResultsEncoding),
        }
    }

    async fn run_angle_query<T>(
        &self,
        query_string: String,
        recursive: bool,
        max_results: Option<i64>,
    ) -> Result<Vec<T>, GleanClientError>
    where
        for<'a> T: Deserialize<CompactProtocolDeserializer<std::io::Cursor<&'a [u8]>>>,
    {
        let mut serialized_results: Vec<Vec<u8>> = Vec::new();
        let (resp_bytes, resp_cont) = self
            .run_single_angle_query(query_string.clone(), recursive, max_results, None)
            .await?;
        serialized_results.extend(resp_bytes);
        let mut cont = resp_cont;
        while cont.is_some() {
            let (resp_bytes, resp_cont) = self
                .run_single_angle_query(query_string.clone(), recursive, max_results, cont)
                .await?;
            serialized_results.extend(resp_bytes);
            cont = resp_cont;
        }
        let results: Vec<T> = serialized_results
            .iter()
            .map(fbthrift::compact_protocol::deserialize)
            .collect::<Result<Vec<_>, _>>()?;
        Ok(results)
    }

    async fn run_single_angle_query(
        &self,
        query_string: String,
        recursive: bool,
        max_results: Option<i64>,
        continuation: Option<UserQueryCont>,
    ) -> Result<(Vec<Vec<u8>>, Option<UserQueryCont>), GleanClientError> {
        let max_results = max_results.or(Some(QUERY_MAX_RESULTS));
        let encodings = vec![UserQueryEncoding::compact(UserQueryEncodingCompact {
            expand_results: recursive,
        })];
        let options = UserQueryOptions {
            syntax: QuerySyntax::ANGLE,
            continuation,
            recursive,
            max_results,
            ..UserQueryOptions::default()
        };
        let query = UserQuery {
            query: query_string,
            options: Some(options),
            encodings,
            client_info: Some(self.client_info.clone()),
            ..UserQuery::default()
        };
        let permit = self.semaphore.acquire().await?;
        let resp: UserQueryResults = self.client.userQuery(&self.repo, &query).await?;
        drop(permit);
        let results: UserQueryEncodedResults = resp.results;
        let cont: Option<UserQueryCont> = resp.continuation;
        match results {
            UserQueryEncodedResults::compact(compact_results) => Ok((compact_results.facts, cont)),
            _ => Err(GleanClientError::UnsupportedResultsEncoding),
        }
    }
}

impl From<AddrParseError> for GleanClientError {
    fn from(err: AddrParseError) -> Self {
        GleanClientError::InternalError(format!("Addr parsing error: {}", err))
    }
}

impl From<SystemTimeError> for GleanClientError {
    fn from(err: SystemTimeError) -> Self {
        GleanClientError::InternalError(format!("System time error: {}", err))
    }
}

impl From<anyhow::Error> for GleanClientError {
    fn from(err: anyhow::Error) -> Self {
        GleanClientError::InternalError(format!("Internal error: {}", err))
    }
}

impl From<UserQueryError> for GleanClientError {
    fn from(err: UserQueryError) -> Self {
        GleanClientError::QueryError(format!("User query error: {}", err))
    }
}

impl From<UserQueryFactsError> for GleanClientError {
    fn from(err: UserQueryFactsError) -> Self {
        GleanClientError::QueryError(format!("User query facts error: {}", err))
    }
}

impl From<AcquireError> for GleanClientError {
    fn from(err: AcquireError) -> Self {
        GleanClientError::InternalError(format!("Semaphore acquire error: {}", err))
    }
}

impl From<NonthrowingFunctionError> for GleanClientError {
    fn from(err: NonthrowingFunctionError) -> Self {
        GleanClientError::ThriftFunctionError(format!("Thrift function error: {}", err))
    }
}

impl From<ConfigeratorError> for GleanClientError {
    fn from(err: ConfigeratorError) -> Self {
        GleanClientError::InternalError(format!("Configerator error: {}", err))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use glean_schema_src::types::File;
    use glean_schema_testinfra::types::AssemblyId;

    #[test]
    fn internal() {
        let name = Glean::glean_name::<AssemblyId>();
        assert_eq!(name, "testinfra.AssemblyId");
        let name = Glean::glean_name::<File>();
        assert_eq!(name, "src.File");
    }
}
