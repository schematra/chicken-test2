# test2

A friendly test suite for [CHICKEN Scheme](https://call-cc.org) 5, with
**optional JUnit XML output** so CI systems (GitHub Actions, GitLab, Jenkins,
CircleCI, ...) can capture your results and show tests run / passed / failed /
skipped with per-test detail.

`test2` is a **drop-in superset** of Alex Shinn's
[`test`](https://wiki.call-cc.org/eggref/5/test) egg. The entire `test` API is
re-exported unchanged and the console output is byte-for-byte identical; the
only additions are `current-test-xml-output` and `test-write-xml`. If you know
`test`, you already know `test2`.

## Installation

```sh
chicken-install test2
```

## Usage

Exactly like the `test` egg, but `import test2`:

```scheme
(import test2)

(test-begin "arithmetic")
(test "addition" 4 (+ 2 2))          ; compare expected vs. actual
(test-assert "truthy" (> 3 0))       ; assert a true value
(test-error "throws" (car '()))      ; expect an error
(test-group "nested"
  (test "reverse" '(c b a) (reverse '(a b c))))
(test-end "arithmetic")

(test-exit)                          ; exit non-zero if anything failed
```

See the [`test` egg documentation](https://wiki.call-cc.org/eggref/5/test) for
the full API (`test`, `test-assert`, `test-error`, `test-group`, filters via
`TEST_FILTER`/`TEST_REMOVE`, comparators, verbosity, etc.). All of it applies.

## JUnit XML output

XML output is **off by default** and adds zero overhead when unused. Enable it
by naming an output file, in one of three ways:

**1. Environment variable (recommended for CI):**

```sh
TEST_XML=test-results/junit.xml csi -s run.scm
# or TEST_XML_FILE=... — both are accepted
```

**2. The `current-test-xml-output` parameter, before your tests run:**

```scheme
(import test2)
(current-test-xml-output "test-results/junit.xml")
(test-begin "suite")
...
(test-end "suite")
(test-exit)
```

**3. Use `-` to write the report to stdout** instead of a file:

```sh
TEST_XML=- csi -s run.scm
```

When enabled, the report is written automatically when the program exits (an
`on-exit` hook, also triggered by `test-exit`). You can also write it yourself
at a chosen moment / path with `(test-write-xml "somewhere.xml")`.

> **Important:** output must be enabled *before* the tests run — that is what
> switches on result recording. Calling `test-write-xml` without having enabled
> output first produces an empty report.

### What the report looks like

Each `test-group` becomes a `<testsuite>`; nested groups get a
slash-separated name (`lists/nested`). Every `test` becomes a `<testcase>`
whose `classname` is the suite path. Failures, errors and skipped tests get
`<failure>`, `<error>` and `<skipped/>` children, and all names/messages are
XML-escaped.

```xml
<?xml version="1.0" encoding="UTF-8"?>
<testsuites name="test2" tests="10" failures="1" errors="1" skipped="0" time="0.0">
  <testsuite name="arithmetic" tests="4" failures="1" errors="0" skipped="0" time="0.0">
    <testcase name="addition" classname="arithmetic" time="0.0"/>
    <testcase name="off by one" classname="arithmetic" time="0.0">
      <failure message="expected 5 but got 4" type="failure">source: (+ 2 2)
expected: 5
got: 4
</failure>
    </testcase>
  </testsuite>
  <testsuite name="lists/nested" tests="2" failures="0" errors="0" skipped="0" time="0.0">
    <testcase name="reverse" classname="lists/nested" time="0.0"/>
  </testsuite>
</testsuites>
```

## GitHub Actions

The report is standard JUnit XML, so any JUnit consumer works. A complete
workflow lives in [`.github/workflows/ci.yml`](.github/workflows/ci.yml); the
core of it is:

```yaml
- name: Install CHICKEN Scheme
  run: sudo apt-get update && sudo apt-get install -y chicken-bin

# sudo: the distro package's egg repository under /var/lib/chicken is
# only writable by root.
- name: Build and install egg
  run: sudo chicken-install

# Run the suite and capture the report. csi runs unprivileged so the file
# is ours to read; a real test failure fails the job.
- name: Run suite and capture JUnit XML
  run: |
    mkdir -p test-results
    TEST_XML=test-results/junit.xml csi -s tests/run.scm

- name: Publish test report
  uses: mikepenz/action-junit-report@v5
  if: always()
  with:
    report_paths: 'test-results/junit.xml'
    include_passed: true
```

For the report step to create a check, grant the workflow token
`checks: write` (the default `GITHUB_TOKEN` is read-only):

```yaml
permissions:
  contents: read
  checks: write
```

That surfaces the run/passed/failed/skipped counts and per-test failure detail
directly in the GitHub Checks tab and the job summary. Any equivalent action
(`dorny/test-reporter`, `EnricoMi/publish-unit-test-result-action`, ...) reads
the same file. The full workflow also builds
[`examples/mixed.scm`](examples/mixed.scm) — a deliberately failing suite — into
a separate report uploaded as an artifact, so you can see what captured
failures and errors look like without turning the commit's checks red.

## New API

Beyond the full `test` API, `test2` adds:

| Binding | Kind | Description |
| --- | --- | --- |
| `current-test-xml-output` | parameter | Output path for the JUnit report (a filename, or `"-"` for stdout). Defaults to the `TEST_XML` / `TEST_XML_FILE` environment variable, else `#f` (disabled). Set it *before* running tests. |
| `test-write-xml` | procedure | `(test-write-xml [file])` writes the report now. With `file`, writes there; with no argument, writes to `current-test-xml-output` at most once. Called automatically at exit. |
| `test-xml-reset!` | procedure | `(test-xml-reset!)` discards everything recorded so far, so a later run starts a fresh report. Useful for emitting several independent reports from one process, or dropping warm-up tests before capturing the real ones. |

## Development

```sh
chicken-install -test     # build, install, and run tests/run.scm
```

## License

BSD, same as the upstream `test` egg. Copyright (c) 2007-2014 Alex Shinn;
JUnit XML output added in this fork.
