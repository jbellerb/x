mod timer;
mod ui;

use std::cell::RefCell;
use std::fmt::Debug;
use std::rc::Rc;
use std::sync::mpsc;
use std::thread;

use crate::ui::{UIMessage, UIManager};

use anyhow::{anyhow, Error, Result};
use deno_core::{BufVec, JsRuntime, Op, OpFn, OpState, ZeroCopyBuf};
use log::{log, trace, Level};
use serde::{de::DeserializeOwned, Deserialize, Serialize};
use tui::{backend::CursesBackend, Terminal};

#[derive(Deserialize, Debug)]
struct LogMessage {
    level: Level,
    message: String,
}

pub fn json_op_sync<V, R, F>(op_fn: F) -> Box<OpFn>
where
    V: DeserializeOwned + Debug,
    R: Serialize,
    F: Fn(&mut OpState, V, &mut [ZeroCopyBuf]) -> Result<R, Error> + 'static,
{
    Box::new(move |state: Rc<RefCell<OpState>>, mut bufs: BufVec| -> Op {
        let result = serde_json::from_slice(&bufs[0])
            .map_err(Error::from)
            .and_then(|args| {
                trace!("recieved message {:?}", args);
                op_fn(&mut state.borrow_mut(), args, &mut bufs[1..])
            });
        let value = match result {
            Ok(v) => serde_json::json!({ "ok": v }),
            Err(err) => serde_json::json!({
              "err": {
                "className": (state.borrow().get_error_class_fn)(&err),
                "message": err.to_string(),
              }
            }),
        };
        Op::Sync(serde_json::to_vec(&value).unwrap().into_boxed_slice())
    })
}

fn create_js_runtime() -> JsRuntime {
    let mut runtime = JsRuntime::new(Default::default());
    runtime.register_op("log", json_op_sync(op_log));
    timer::init(&mut runtime);
    runtime
}

fn op_log(
    _state: &mut OpState,
    json: LogMessage,
    _zero_copy: &mut [ZeroCopyBuf],
) -> Result<(), Error> {
    log!(json.level, "{}", json.message);

    Ok(())
}

fn main() -> Result<()> {
    pretty_env_logger::init();

    let (tx, mut rx) = mpsc::channel::<UIMessage>();

    let ui = thread::spawn(move || ui::work(rx));

    //let backend = CursesBackend::new().ok_or_else(|| anyhow!("failed to init ncurses"))?;
    //let terminal = Terminal::new(backend)?;
    //let mut manager = UIManager::new(terminal);

    //manager.render()?;

    let mut runtime = create_js_runtime();

    runtime
        .execute("<runtime>", include_str!("runtime.js"))
        .unwrap();
    runtime
        .execute(
            "<bundle>",
            include_str!(concat!(env!("OUT_DIR"), "/index.bundle.js")),
        )
        .unwrap();

    Ok(())
}
