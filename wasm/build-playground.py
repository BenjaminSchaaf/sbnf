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

    args = parser.parse_args()

    if not os.path.exists(args.DEST):
        print('Destination path does not exist')
        return 1

    os.chdir(os.path.dirname(os.path.realpath(__file__)))

    subprocess.run(['wasm-pack', 'build', '--target', 'web'], check=True)

    template = Template(open('playground.html.jinja2', 'r').read())

    spectre_css = open('spectre.min.css', 'r').read()

    dest = os.path.join(args.DEST, 'playground.html')
    open(dest, 'w').write(template.render({ 'spectre_css': spectre_css }))

    shutil.copy('pkg/sbnf_wasm.js', os.path.join(args.DEST, 'sbnf_wasm.js'))
    shutil.copy('pkg/sbnf_wasm_bg.wasm', os.path.join(args.DEST, 'sbnf_wasm_bg.wasm'))

    subprocess.run(['git', 'status'], check=True, cwd=args.DEST)

if __name__ == '__main__':
    sys.exit(main(sys.argv))
