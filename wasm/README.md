# Building

```bash
# In the root of the project
git worktree add gh-pages gh-pages

cd wasm

# Optionally add --release for a build with minifiers
python3 build-playground.py ../gh-pages
```

## Dependencies

* wasm-pack
* uglify-js (only needed for release)
* html-minifier (only needed for release)

```bash
# Suggest installing in your user folder on linux:
npm config set prefix '~/.local/'

npm install uglify-js -g
npm install html-minifier -g

cargo install wasm-pack
```
