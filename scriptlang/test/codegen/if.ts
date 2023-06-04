const base_if = () => {
  let a = 1;
  if (a == 2) {
    a = 3;
  } else {
    a = 4;
  }
};

const if_only_then = () => {
  let a = 1;
  if (a == 2) {
    a = 3;
  }
};

const nesting_if = () => {
  let a = 1;
  if (a > 2) {
    if (a > 3) {
      a = 4;
    } else {
      a = 5;
    }
  } else {
    if (a > 6) {
      a = 7;
    } else {
      a = 8;
    }
  }
};

const elseif = () => {
  let a = 1;
  if (a > 2) {
    a = 3;
  } else if (a > 4) {
    a = 5;
  } else {
    a = 6;
  }
};
