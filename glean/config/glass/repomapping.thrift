namespace hs Glean.Glass

typedef string RepoName
typedef string GleanDBName
typedef string Language

struct DbSelector {
  1: GleanDBName name
  2: Language language
  3: optional string branch
} (hs.prefix = "")

struct RepoMapping {
  1: map<RepoName,list<DbSelector>> indices
} (hs.prefix = "")
