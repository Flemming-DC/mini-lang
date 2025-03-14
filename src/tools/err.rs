use std::{error::Error, fmt, io, process::{ExitCode, ExitStatus}};
use std::sync::Mutex;
use crate::dbg_assert;

// -------- Assert No Program Error -------- //

static assert_valid_program: Mutex<bool> = Mutex::new(false);
pub fn set_valid_program_assertion(on: bool) {
    *assert_valid_program.lock().expect("mutex error presumes another compiler error") = on;
}
pub fn get_valid_program_assertion() -> bool {
    assert_valid_program.lock().expect("mutex error presumes another compiler error").clone()
}


// -------- Program Error -------- //
#[derive(Debug)] pub struct ProgramError { pub msg: String }

impl Error for ProgramError {}
impl fmt::Display for ProgramError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ProgramError: {}", self.msg)
    }
}
impl From<io::Error> for ProgramError {
    /// Use with caution. IO errors usually aren't program errors.
    fn from(error: io::Error) -> Self {
        ProgramError { msg: error.to_string() }
    }
}


#[macro_export] macro_rules! program_error {
    ($($arg:tt)*) => {{
        use crate::tools::err::{ProgramError, get_valid_program_assertion};
        #[cfg(debug_assertions)] {
            if get_valid_program_assertion() {
                panic!("{}", format!($($arg)*));
        }};
        #[allow(unreachable_code)] Err(ProgramError {msg: format!($($arg)*) })
    }}
}

pub type Fallible<T> = Result<T, ProgramError>;



// -------- Cmd Error -------- //
#[derive(Debug)] pub struct CmdError { pub msg: String }

impl Error for CmdError {}
impl fmt::Display for CmdError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "CmdError: {}", self.msg)
    }
}
impl From<io::Error> for CmdError {
    /// Use with caution. IO errors might not be cmd errors.
    fn from(error: io::Error) -> Self {
        CmdError { msg: error.to_string() }
    }
}

pub fn cmd_result(status: ExitStatus) -> Result<(), CmdError> {
    return if status.success() 
             { Ok(()) }
        else { Err(CmdError { msg: status.to_string() }) };
}


// -------- Input Error -------- //
#[derive(Debug)] pub struct InputError { pub msg: String }

impl Error for InputError {}
impl fmt::Display for InputError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "InputError: {}", self.msg)
    }
}
impl From<io::Error> for InputError {
    /// Use with caution. IO errors might not be cmd errors.
    fn from(error: io::Error) -> Self {
        InputError { msg: error.to_string() }
    }
}
#[macro_export] macro_rules! input_error {
    ($($arg:tt)*) => {{
        use crate::tools::err::{InputError, get_valid_program_assertion};
        #[cfg(debug_assertions)] {
            if get_valid_program_assertion() {
                panic!("{}", format!($($arg)*));
        }};
        #[allow(unreachable_code)] Err(InputError {msg: format!($($arg)*) })
    }}
}

// -------- Compilation Error -------- //
pub struct CompilationError { pub msg: String }

impl Error for CompilationError {}
impl fmt::Display for CompilationError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.msg)
    }
}
impl fmt::Debug for CompilationError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.msg)
    }
}
impl From<ProgramError> for CompilationError {
    fn from(error: ProgramError) -> Self {
        CompilationError { msg: error.to_string() }
    }
}
impl From<CmdError> for CompilationError {
    fn from(error: CmdError) -> Self {
        CompilationError { msg: error.to_string() }
    }
}
impl From<InputError> for CompilationError {
    fn from(error: InputError) -> Self {
        CompilationError { msg: error.to_string() }
    }
}
impl From<io::Error> for CompilationError {
    /// Use with caution. IO errors usually aren't program errors.
    fn from(error: io::Error) -> Self {
        CompilationError { msg: error.to_string() }
    }
}




