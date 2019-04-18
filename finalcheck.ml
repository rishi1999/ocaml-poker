module M = Authors

let () =
  if List.length M.hours_worked < 1
  || List.exists (fun n -> n < 0) M.hours_worked
  then print_string
      "===========================================================\n\
       WARNING: \n\
       You have not set hours_worked. Please read the submission\n\
       instructions in the assignment handout.\n\
       ===========================================================\n"
