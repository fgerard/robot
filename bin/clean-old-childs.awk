BEGIN {
  print("CLEAN OLD CHILDS(1), parameters, ppid: " ppid ", hour: " hour ", minute: " minute)
  hour-=1;
}
$3==ppid || $3==1 {
  date=sprintf("%02d:%02d", hour, minute);
  print("CLEAN OLD CHILDS(2) date: " date " TESTING: " $5)
  if ((($5>"A") || ($5<date)) && (match($0,"chrome|firefox") != 0)) {
    print("CLEAN OLD CHILDS(3) Killing process: " $2 " child of " ppid "\n");
    system("kill -9 " $2);
  }
}
