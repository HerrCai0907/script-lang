import lit

# name: The name of this test suite.
config.name = "scriptlang"

config.test_format = lit.formats.ShTest(0)

# suffixes: A list of file extensions to treat as test files.
config.suffixes = [
    ".ts",
]
config.excludes = ["index.d.ts"]

# test_source_root: The root path where tests are located.
config.test_source_root = os.path.dirname(__file__)

# test_exec_root: The root path where tests should be run.
config.test_exec_root = "."
