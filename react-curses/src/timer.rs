use std::cell::RefCell;
use std::rc::Rc;

use crate::json_op_sync;

use anyhow::Error;
use deno_core::{json_op_async, BufVec, JsRuntime, OpState, ZeroCopyBuf};
use serde_json::{json, Value};
use tokio::time::{sleep, Duration, Instant};

pub fn init(runtime: &mut JsRuntime) {
    {
        let op_state = runtime.op_state();
        let mut state = op_state.borrow_mut();
        state.put::<Instant>(Instant::now());
    }

    runtime.register_op("sleep", json_op_async(op_sleep));
    runtime.register_op("now", json_op_sync(op_now));
}

async fn op_sleep(
    state: Rc<RefCell<OpState>>,
    args: Value,
    _zero_copy: BufVec,
) -> Result<Value, Error> {
    let args = serde_json::from_value(args)?;
    let duration = Duration::from_millis(args);

    sleep(duration).await;

    Ok(json!({}))
}

fn op_now(state: &mut OpState, _json: (), _zero_copy: &mut [ZeroCopyBuf]) -> Result<u64, Error> {
    let start_time = state.borrow::<Instant>();
    let time = start_time.duration_since(Instant::now());

    Ok(time.as_millis() as u64)
}
