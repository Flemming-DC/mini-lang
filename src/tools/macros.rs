


/// this is a genuie debug_assert. As opposed to the builtin BS that uses an if check in release build.
#[macro_export] macro_rules! dbg_assert { 

    ($arg:expr $(,$msg:expr)?) => {
        #[cfg(debug_assertions)] {
            if !($arg) {
                use std::path::Path;
                let filename: &str = Path::new(std::file!()).file_name().expect("every file has a filename")
                    .to_str().expect("assume valid unicode");
            
                eprintln!("ASSERTION FAILED [{}:{}:{}] \nCondition: {}",
                    filename, std::line!(), std::column!(), std::stringify!($arg));
                $(
                   eprintln!("Message:   {:?}", $msg);
                )?
                use std::hint::black_box;
                black_box(unsafe { std::arch::asm!("int3"); });
            }
        }
    // ($($arg:tt)* $(,$msg:expr)?) => {
    //     #[cfg(debug_assertions)] {
    //         if !($($arg)*) {
    //             eprintln!("ASSERTION FAILED [{}:{}:{}] \nCondition: {}",
    //                 filename, std::line!(), std::column!(), std::stringify!($($arg)*));
    //             $(
    //                 eprintln!("Message: {}", $msg:expr);
    //             )?
    //             use std::hint::black_box;
    //             black_box(unsafe { std::arch::asm!("int3"); });
    //         }
    //     }
    };
}

#[macro_export] macro_rules! if_dbg {
    ($($arg:tt)*) => {
        #[cfg(debug_assertions)] {$($arg)*}
    };
}

#[macro_export] macro_rules! brk { 
    () => {
        #[cfg(debug_assertions)] {
            use std::hint::black_box;
            black_box(unsafe { std::arch::asm!("int3"); })
        };
    };
}


#[macro_export] macro_rules! brk_if { 
    ($condition:expr) => {
        #[cfg(debug_assertions)] {
            use std::hint::black_box;
            if $condition { black_box(unsafe { std::arch::asm!("int3"); }) }
        };
    };
}


// #[cfg(debug_assertions)]
#[macro_export] macro_rules! P {
    // Cupy paste of dbg! with minor canges. 
    // (1) Verbose formatting is replaced with compact formatting.
    // (2) 
    // --------------------------------
    // NOTE: We cannot use `concat!` to make a static string as a format argument
    // of `eprintln!` because `file!` could contain a `{` or
    // `$val` expression could be a block (`{ .. }`), in which case the `eprintln!`
    // will be malformed.
    () => {
        use std::path::Path;
        let filename: &str = Path::new(std::file!()).file_name().expect("every file has a filename")
            .to_str().expect("assume valid unicode");

        eprintln!("[{}:{}:{}]", filename, std::line!(), std::column!())
    };
    ($val:expr $(,)?) => {
        // Use of `match` here is intentional because it affects the lifetimes
        // of temporaries - https://stackoverflow.com/a/48732525/1063961
        match $val {
            tmp => {
                use std::path::Path;
                let filename: &str = Path::new(std::file!()).file_name().expect("every file has a filename")
                    .to_str().expect("assume valid unicode");

                eprintln!("[{}:{}:{}] {} = {:#?}",
                    filename, std::line!(), std::column!(), std::stringify!($val), &tmp);
                tmp
            }
        }
    };
    // ($($val:expr),+ $(,)?) => {
    //     ($($crate::Q!($val)),+,)
    // };
    ($first:expr $(, $rest:expr)*) => {
        match $first {
            tmp => {
                use std::path::Path;
                let filename: &str = Path::new(std::file!()).file_name().expect("every file has a filename")
                    .to_str().expect("assume valid unicode");

                eprintln!("[{}:{}:{}] {} = {:#?}",
                    filename, std::line!(), std::column!(), std::stringify!($first), &tmp);
                // tmp
            }
        }

        ($(match $rest {
            tmp => {
                use std::path::Path;
                let filename: &str = Path::new(std::file!()).file_name().expect("every file has a filename")
                    .to_str().expect("assume valid unicode");

                let prefix = format!("[{}:{}:{}] ", filename, std::line!(), std::column!());
                let blanks = " ".repeat(prefix.len());
                eprintln!("{}{} = {:#?}",
                blanks, std::stringify!($rest), &tmp);
                tmp
            }
        }),+,)
    };
}



#[cfg(debug_assertions)]
#[macro_export] macro_rules! Piter {
    ($v:expr $(,)?) => {
        match $v {
            tmp => {
                use std::path::Path;
                let filename: &str = Path::new(std::file!()).file_name().expect("every file has a filename")
                    .to_str().expect("assume valid unicode");

                eprintln!("[{}:{}:{}] {} = [",
                    filename, std::line!(), std::column!(), std::stringify!($val));
                for val in $v {
                    eprintln!("{:?}", val);
                }
                eprintln!("]");
                tmp
            }
        }
    };
    // ($first:expr $(, $rest:expr)*) => {
    // };
}


#[macro_export] macro_rules! func_name {
    () => {{
        fn f() {}
        fn type_name_of<T>(_: T) -> &'static str {
            std::any::type_name::<T>()
        }
        let name = type_name_of(f);

        // Find and cut the rest of the path
        match &name[..name.len() - 3].rfind(':') {
            Some(pos) => &name[pos + 1..name.len() - 3],
            None => &name[..name.len() - 3],
        }
    }};
}

#[macro_export] macro_rules! func_log {
    ($msg: expr) => {{
        &format!("{}| {}", func_name!(), $msg)
    }};
}
