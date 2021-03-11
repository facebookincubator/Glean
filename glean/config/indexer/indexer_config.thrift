namespace hs Glean
namespace php glean

struct Options {
  1: bool includeDependencies = true;
    // Include (transitive) dependencies of specified targets
}

struct Config {
  1: list<string> targetBlacklist = [];
  2: map<string, Options> subdirOptions = {};
}
