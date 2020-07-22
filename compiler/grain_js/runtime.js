//Provides: unix_stat
function unix_stat() {
  console.log(arguments);
  throw new Error('Unix stat')
}

//Provides: unix_lstat
function unix_lstat() {
  console.log(arguments);
  throw new Error('Should not reach `Unix.lstat`');
}

//Provides: unix_readlink
function unix_readlink() {
  console.log(arguments);
  throw new Error('Should not reach `Unix.readlink');
}

//Provides: unix_mkdir
function unix_mkdir() {
  console.log(arguments);
  return 0;
}
