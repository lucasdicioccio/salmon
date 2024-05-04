# Salmon, a new approach to xyz-dependencies

note: blog post in the making

- salmon-core: core library, to make dependencies of about anything
- salmon-ops: ops library, for IO-heavy system tasks such as found in provisioning and ci/cd tools, includes a number of primitives, we try to stay low-cabal-deps but allow to pull-in cabal-deps for tasks that are pretty deep (e.g., generating and signing certificates)
- salmon-ops-recipes: same as above, except that it is the home for heavy recipes where a number of conventions start to be enforced (e.g., whether we ship migration info and then migrate-db locally or whether we use a connstring from a distant place)
- salmon-apps: blessed and project-useful binaries
