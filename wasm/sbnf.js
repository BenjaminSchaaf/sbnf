const { compile } = wasm_bindgen;

const init = wasm_bindgen('./sbnf.wasm');

self.onmessage = async(event) => {
  await init;

  const [name, code, args, quiet, debug] = event.data;

  const result = compile(name, code, args, quiet, debug);

  syntax = result.syntax();
  messages = result.messages();

  self.postMessage([syntax, messages]);
};
