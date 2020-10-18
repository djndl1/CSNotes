use std::io::{stdin, stdout, Stdin, StdinLock, BufRead};
use std::vec;
use std::cmp::PartialOrd;

fn read_code(cin: &mut dyn BufRead) -> String {
    let mut line = String::new();
    let mut ret = cin.read_line(&mut line).unwrap();
    while ret != 0 && line.trim().is_empty() {
        line.clear();
        ret = cin.read_line(&mut line).unwrap();
    }
    line.trim().to_string()
}

fn next_permutation<T: PartialOrd>(perm: &mut Vec<T>) -> bool {
    let mut k = perm.len() - 1;
    while k > 0 {
        if perm[k-1] < perm[k] {
            break;
        }
        k -= 1;
    }
    if k == 0 {
        return false;
    }

    let mut l = perm.len() - 1;
    while l > k - 1 {
        if perm[k-1] < perm[l] {
            break;
        }
        l -= 1;
    }
    perm.swap(k-1, l);

    let part = &mut perm[k..];
    part.reverse();

    return true;
}


fn main() {
    let cin = stdin();
    let mut sin = cin.lock();
    loop {
        let code = read_code(&mut sin);
        if code.is_empty() || code == "#" {
            break;
        }

        let mut code_list: Vec<char> = code.chars().collect();
        if next_permutation(&mut code_list) {
            let successor: String = code_list.iter().collect();
            println!("{}", successor);
        } else {
            println!("No Successor");
        }
    }
}
