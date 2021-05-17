use fbinit::FacebookInit;
use glean_client::{GleanClient, GleanConfig};
use glean_client_config::ClientConfig;
use glean_schema_cxx1::types::*;
use glean_service::{HostPort, Service};
use structopt::StructOpt;
use tracing::Level;
use tracing_subscriber::FmtSubscriber;

#[derive(StructOpt, Debug)]
// #[structopt(name = "basic")]
struct Args {
    /// SMC tier for Glean.
    #[structopt(long, required_unless_all = &["host","port"], conflicts_with_all = &["host","port"])]
    tier: Option<String>,

    /// Host that's running the service.
    #[structopt(long, required_unless = "tier", conflicts_with = "tier")]
    host: Option<String>,
    /// Port that's running the service.
    #[structopt(long, required_unless = "tier", conflicts_with = "tier")]
    port: Option<u16>,

    /// Name of the repo to query
    #[structopt(long, default_value = "fbsource")]
    repo_name: String,
    /// Hash of the repo to query [default: latest]
    #[structopt(long)]
    repo_hash: Option<String>,
}

#[fbinit::main]
async fn main(fb: FacebookInit) -> Result<(), Box<dyn std::error::Error>> {
    let subscriber = FmtSubscriber::builder()
        .with_max_level(Level::TRACE)
        .finish();

    tracing::subscriber::set_global_default(subscriber).expect("setting default subscriber failed");

    let options = Args::from_args();
    let config = match options {
        Args {
            tier: Some(t),
            host: None,
            port: None,
            ..
        } => Some(ClientConfig {
            serv: Service::tier(t),
            ..Default::default()
        }),
        Args {
            tier: None,
            host: Some(host),
            port: Some(port),
            ..
        } => Some(ClientConfig {
            serv: Service::hostPort(HostPort {
                host,
                port: port as i32,
            }),
            ..Default::default()
        }),
        _ => None,
    };

    let mut glean_config = GleanConfig::new(fb, options.repo_name);
    if let Some(client_config) = config {
        glean_config = glean_config.with_client_config(client_config);
    }
    if let Some(hash) = &options.repo_hash {
        glean_config = glean_config.with_repo_hash(hash.to_string());
    };
    let glean_client = glean_config.to_client().await?;
    let query = r#"
    cxx1.RecordDefinition {
        declaration = {
            name = {
                name = "Future",
            },
        },
    };
    "#;
    let result: Vec<RecordDeclaration> = glean_client
        .run_angle_query(query.to_string(), false, None)
        .await?;
    println!("Result: {:?}", result);
    Ok(())
}
