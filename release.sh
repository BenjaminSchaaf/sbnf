#!/usr/bin/env bash

set -ex

REPO_DIR=$(dirname "$0")
cd $REPO_DIR

VERSION=$1

if [[ -z "$VERSION" ]]; then
    echo "Please provide a version number"
    exit 1
fi

PART=$2
if [[ -z "$PART" ]]; then
    PART=all
elif [[ $PART != 'website' && $PART != 'package' && $PART != 'crate' ]]; then
    echo "2nd argument must be 'website', 'package' or 'crate'"
    exit 1
fi

# Check dependencies
if ! type llvm-lipo-14 >/dev/null 2>&1; then
    echo "'llvm-lipo-14' not installed"
    exit 1
fi

if ! type cross >/dev/null 2>&1; then
    echo "'cross' not installed"
    exit 1
fi

if ! type gh >/dev/null 2>&1; then
    echo "'gh' not installed"
    exit 1
fi

# Make sure we've got git setup right
CURRENT_BRANCH=$(git branch --show-current)
if [ "$CURRENT_BRANCH" != 'master' ]; then
    echo "Need to be on master branch"
    exit 1
fi

WEB_PATH=$(git worktree list | grep -F "gh-pages" | awk '{print $1}')
if [[ -z "$WEB_PATH" ]]; then
    echo "Creating gh-pages worktree"
    git worktree add gh-pages gh-pages
    WEB_PATH="gh-pages"
fi

echo "All checks passed"

cargo set-version "$VERSION"

if [[ $PART == 'all' || $PART == 'package' ]]; then
    rm -rf target/sublime-package
    mkdir target/sublime-package

    ARTIFACTS=()

    # Compile CLI for all platforms
    TARGETS=(
        "x86_64-unknown-linux-gnu"
        "aarch64-unknown-linux-gnu"
        "x86_64-apple-darwin"
        "aarch64-apple-darwin"
        "x86_64-pc-windows-msvc"
        "i686-pc-windows-msvc"
    )
    for TARGET in "${TARGETS[@]}"; do
        cross build --target $TARGET --release

        if [[ "$TARGET" == *windows* ]]; then
            cp target/$TARGET/release/sbnf.exe target/sublime-package/sbnf-$TARGET.exe

            zip "target/sbnf-$TARGET.zip" -C target/$TARGET/release sbnf.exe
            ARTIFACTS+=("target/sbnf-$TARGET.zip")
        elif [[ "$TARGET" == *linux* ]]; then
            cp target/$TARGET/release/sbnf target/sublime-package/sbnf-$TARGET

            tar -cJf "target/sbnf-$TARGET.tar.xz" -C target/$TARGET/release sbnf
            ARTIFACTS+=("target/sbnf-$TARGET.tar.xz")
        elif [[ "$TARGET" == *apple* ]]; then
            zip "target/sbnf-$TARGET.zip" -C target/$TARGET/release sbnf
            ARTIFACTS+=("target/sbnf-$TARGET.zip")
        fi
    done

    # Make universal binary for macOS
    mkdir -p target/universal-apple-darwin
    llvm-lipo-14 -create -output target/universal-apple-darwin/sbnf \
        target/x86_64-apple-darwin/release/sbnf target/aarch64-apple-darwin/release/sbnf

    cp target/universal-apple-darwin/sbnf target/sublime-package/sbnf-universal-apple-darwin

    zip -C target/universal-apple-darwin "target/sbnf-universal-apple-darwin.zip" sbnf
    ARTIFACTS+=("target/sbnf-universal-apple-darwin.zip")

    # Compile SBNF syntax
    cargo run --release -- sbnf/sbnf.sbnf -o target/sublime-package/sbnf.sublime-syntax

    # Copy other files
    SYNTAX_SOURCES=(
        "Comments.tmPreferences"
        "sbnf.sublime-completions"
        "sbnf.sublime-settings"
        "sbnf-linux-universal.sh"
        "sbnf-windows-universal.bat"
        "sbnf.sublime-settings"
        "Symbol Index.tmPreferences"
    )
    for F in "${SYNTAX_SOURCES[@]}"; do
        cp "sbnf/$F" target/sublime-package/
    done
    cp sbnf/sbnf.sublime-build-release target/sublime-package/sbnf.sublime-build
    touch target/sublime-package/.no-sublime-package

    zip -r target/SBNF.sublime-package target/sublime-package/*
    ARTIFACTS+=("target/SBNF.sublime-package")

    cat >packages.json <<EOF
{
    "schema_version": "3.0.0",
    "packages": [
        {
            "name": "Package Control",
            "description": "SBNF",
            "author": "Benjamin Schaaf",
            "homepage": "https://github.com/BenjaminSchaaf/sbnf",
            "readme": "https://raw.githubusercontent.com/BenjaminSchaaf/sbnf/master/README.md",
            "releases": [
                {
                    "sublime_text": ">4077",
                    "version": "$VERSION",
                    "url": "https://github.com/BenjaminSchaaf/sbnf/releases/download/$VERSION/SBNF.sublime-package",
                    "date": "$(date --iso-8601=seconds)"
                }
            ]
        },
    ]
}
EOF
fi

if [[ $PART == 'all' || $PART == 'website' ]]; then
    # Build website
    python3 wasm/build-playground.py --release "$WEB_PATH"
fi

# Prepare Release
if [[ $PART == 'all' || $PART == 'crate' || $PART == 'package' ]]; then
    git status
    git add packages.json
    git add Cargo.toml
    git add Cargo.lock
    git add cli/Cargo.toml
    git add wasm/Cargo.toml
    git commit -m "Release $VERSION"
    git tag "$VERSION"
fi

if [[ $PART == 'all' || $PART == 'website' ]]; then
    cd "$WEB_PATH"
    git add -A
    git status
    git commit -m "Release $VERSION"
    cd -
fi

# Do release after confirmation
echo "Release Prepared"
read -p "Do you want to continue? (y/Y): " CONFIRMATION

if [[ $CONFIRMATION == 'y' || $CONFIRMATION == 'Y' ]]; then
    if [[ $PART == 'all' || $PART == 'package' ]]; then
        git push origin "$VERSION"

        gh release create "$VERSION" "${ARTIFACTS[@]}"

        # Push master after release to make sure packages.json is always valid
        git push origin master
    fi

    if [[ $PART == 'all' || $PART == 'crate' ]] then
        cargo publish -p sbnf
        cargo publish -p sbnfc
    fi

    if [[ $PART == 'all' || $PART == 'website' ]]; then
        cd "$WEB_PATH"
        git push
        cd -
    fi
else
    echo "Cancelled. Cleanup is left to you"
    exit 1
fi
