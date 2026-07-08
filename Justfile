ghc_options_for_build := "-Werror"

[doc("Format all source code")]
format:
  @echo "Formatting all source code"
  stack exec -- ormolu --mode inplace $(find src test app -type f -name "*.hs")

[doc("Lint and refactor all source code according to HLint rules")]
lint:
  @echo "Linting and refactoring all source code"
  ./lint.sh

[doc("Build project with Stack, and treat all diagnostics as errors")]
build:
  @echo "Building project"
  stack clean
  stack build --ghc-options "{{ghc_options_for_build}}"

[doc("Run test suite with Stack and Tasty")]
test:
  @echo "Running test suite"
  stack test --ghc-options "{{ghc_options_for_build}}"  --test-arguments "-j 1"

[doc("Run tests that match a pattern with Stack and Tasty")]
test_pattern pattern:
  @echo "Running all tests that match pattern: {{pattern}}"
  stack test --ghc-options "{{ghc_options_for_build}}" --test-arguments "-j 1 -p \"{{pattern}}\""

[doc("Run test suite with Stack and Tasty, and update all golden files")]
test_update_goldenfiles:
  @echo "Running test suite and updating golden files"
  stack test --ghc-options "{{ghc_options_for_build}}" --test-arguments "-j 1 --accept"

[doc("Run all code checks")]
check: format build lint test

[doc("Query the Hoogle database with a Hoogle-style query")]
hoogle query:
  ./hoogle.sh "{{query}}"
