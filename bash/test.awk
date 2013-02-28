## This script counts the frequency of a word and saves it into a map

## It keeps the 10 best words so far into a sorted list (smallest to biggest)
## and only outputs words which has a higher count than the first of 10 best so
## far. And it updates the sorted list:
## insert on correct position (sorted) if there are < 10 elements in list
## kick smallest and insert than the new value

function sort(best_sofar) {
  for (i = 0; i < length(best_sofar)-1; i++) {
    if (best_sofar[i] > best_sofar[i+1]) {
      temp = best_sofar[i+1]
      best_sofar[i+1] = best_sofar[i]
      best_sofar[i] = temp
    } else { return; }
  }
}

function insert(best_sofar,j) {
  for (i = 0; i < length(best_sofar); i++) {
    if (best_sofar[i] > j) {
      for (k = length(best_sofar); k > i; k--) {
        best_sofar[k] = best_sofar[k-1]
      }
      best_sofar[i] = j
      return
    }
  }
  best_sofar[length(best_sofar)] = j
}

{count[$1]++}
END {
  best_sofar[0] = 0
  for (j in count) {
    if (length(best_sofar) < 10) {

      insert(best_sofar,count[j])
      print count[j],j;

    } else {
      if (count[j] == best_sofar[0]) {
        print count[j],j;
      } else {
        if (count[j] > best_sofar[0]) {

          print count[j],j;
          best_sofar[0] = count[j];
          sort(best_sofar)

        }
      }
    }
  }
}

