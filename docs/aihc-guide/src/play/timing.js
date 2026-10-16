export async function measure(stage, action, report, reused = false) {
  const start = performance.now();
  try {
    return await action();
  } finally {
    report({ stage, milliseconds: performance.now() - start, reused });
  }
}
