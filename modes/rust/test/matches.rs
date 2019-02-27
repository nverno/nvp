fn main() {
  struct Point { 
    x: i32, 
    y: i32, 
  }
  
  let point = Point { x: 1, y: 2};

  match point {
    _ => {  }
  }
  
  let x = Some("value");
  let y: Option<u32> = Some(2);

}
