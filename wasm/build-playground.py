import os
import sys
import subprocess
from jinja2 import Template
import argparse
import base64
import shutil


def main(argv):
    parser = argparse.ArgumentParser()
    parser.add_argument('DEST', default='../gh-pages')
    parser.add_argument('--release', default=False, action='store_true')

    args = parser.parse_args()

    if not os.path.exists(args.DEST):
        print('Destination path does not exist')
        return 1

    DEST = os.path.realpath(args.DEST)

    os.chdir(os.path.dirname(os.path.realpath(__file__)))

    # Compile WASM
    wasmpack_args = [
        'wasm-pack', 'build',
        '--target', 'no-modules',
        '--no-typescript',
    ]
    if not args.release:
        wasmpack_args.append('--profiling')
    wasmpack_args += [
        '.',
        '-Z', 'build-std=std,panic_abort',
        '-Z', 'build-std-features=panic_immediate_abort'
    ]
    subprocess.run(wasmpack_args, check=True)

    # Install WASM
    shutil.copy('pkg/sbnf_wasm_bg.wasm', os.path.join(DEST, 'sbnf.wasm'))

    # Compile HTML
    template = Template(open('playground.html.jinja2', 'r').read())
    html = template.render({})
    open('pkg/playground.html', 'w').write(html)

    # Install (Minify) HTML
    dest = os.path.join(DEST, 'playground.html')
    if args.release:
        subprocess.run(['html-minifier', '--minify-css', '--minify-js=-cm', '-o', dest, 'pkg/playground.html'], check=True)
    else:
        shutil.copy('pkg/playground.html', dest)

    # Cat JS
    wasm_js = open('pkg/sbnf_wasm.js', 'r').read()
    sbnf_js = open('sbnf.js', 'r').read()
    open('pkg/sbnf.js', 'w').write(wasm_js + sbnf_js)

    # Install (Minify) JS
    dest = os.path.join(DEST, 'sbnf.js')
    if args.release:
        subprocess.run(['uglifyjs', '--module', '-cm', '-o', dest, 'pkg/sbnf.js'], check=True)
    else:
        shutil.copy('pkg/sbnf.js', dest)

    # Print git status
    subprocess.run(['git', 'status'], check=True, cwd=DEST)

if __name__ == '__main__':
    sys.exit(main(sys.argv))
