# coverage-check

A simple CLI tool for extracting and checking coverage information from a
coverage report generated via `stack test --coverage`.

```bash
$ coverage-check --help
Header for command line arguments

Usage: coverage-check [--version] [--help] [-v|--verbose]
                      [-e|--environment-file DOTENV_FILE]
                      [-c|--config CONFIG_FILE] PROJECT_PATH
  Program description, also for command line arguments

Available options:
  --version                Show version
  --help                   Show this help text
  -v,--verbose             Verbose output?
  -e,--environment-file DOTENV_FILE
                           Environment file to load during startup
  -c,--config CONFIG_FILE  Config file to load during startup
  PROJECT_PATH             The path to the project to check
```

