# register_testing

Simple skeleton for register test case generation.

## How to use

Set path to Python-interpreter to `Justfile`.

Build testcases.

`just build`

Build testcases without parsing.

`just build-without-parsing`

Build and run (**will cause segmentation fault 8-)**).

`just run`

## Dependencies

- `cargo`
    - `curl https://sh.rustup.rs -sSf | sh`
- `python3.10`
    - `sudo apt install python3.10`
- `just`
    - `cargo install just`
- `libxml2`
    - `sudo apt install libxml2-dev`
- `libxslt`
    - `sudo apt install libxslt-dev`

### Python modules

`beautifulsoup 4`

`lxml`

`html5lib`

Python dependencies can be installed from `requirements.txt`.

- `pip install -r requirements.txt`

