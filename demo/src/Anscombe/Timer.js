// Timer FFI - Native JS timing functions
// These yield to the event loop properly, avoiding stack overflow.

export const setInterval = (ms) => (action) => () => {
  return window.setInterval(action, ms);
};

export const clearInterval = (id) => () => {
  window.clearInterval(id);
};

export const setTimeout = (ms) => (action) => () => {
  window.setTimeout(action, ms);
};
