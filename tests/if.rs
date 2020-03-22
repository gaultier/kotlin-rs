use kotlin::compile::compile;

#[test]
fn simple_if_expr() {
    let src = String::from("if (1<2) 'o' else 'x';");
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_ref().unwrap(),
        &"(if (< 1 2) 'o' 'x')\n"
    );
}

#[test]
fn multi_if_expr() {
    let src = String::from("if (1<2) 'o' else 'x'\nif (true) \n\n 42 \n else 99");
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_ref().unwrap(),
        &"(if (< 1 2) 'o' 'x')\n(if #t 42 99)\n"
    );
}

#[test]
fn nested_if_expr() {
    let src = String::from("if (1<2) if (99U < 100UL) 'a' else 'b' else 'c'\n");
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_ref().unwrap(),
        &"(if (< 1 2) (if (< 99 100) 'a' 'b') 'c')\n"
    );
}

#[test]
fn if_body_block() {
    let src = String::from("if (1<2) {'a'; 1\n\n true;; 'b'} else {1*3; 'c'}\n");
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_ref().unwrap(),
        &"(if (< 1 2) (begin 'a' 1 #t 'b') (begin (* 1 3) 'c'))\n"
    );
}

#[test]
fn if_with_empty_else_block() {
    let src = String::from("if (1<2) {'a'; 1\n\n true;; 'b'} else {}\n");
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_ref().unwrap(),
        &"(if (< 1 2) (begin 'a' 1 #t 'b') (begin ))\n"
    );
}

#[test]
fn if_with_empty_if_body_block() {
    let src = String::from("if (1<2) {} else {'a'; 1\n\n true;; 'b'}\n");
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_ref().unwrap(),
        &"(if (< 1 2) (begin ) (begin 'a' 1 #t 'b'))\n"
    );
}

#[test]
fn if_with_no_if_body_block() {
    let src = String::from("if (1<2) \n else {'a'; 1\n\n true;; 'b'}\n");
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_ref().unwrap(),
        &"(if (< 1 2) (begin ) (begin 'a' 1 #t 'b'))\n"
    );
}

#[test]
fn if_with_no_else_block() {
    let src = String::from("if (1<2) {'a'; 1\n\n true;; 'b'} else ; \n");
    let mut out: Vec<u8> = Vec::new();

    assert!(compile(src, &mut out).is_ok());
    assert_eq!(
        std::str::from_utf8(&out).as_ref().unwrap(),
        &"(if (< 1 2) (begin 'a' 1 #t 'b') (begin ))\n"
    );
}
