funclub_wordcount
=================

Simple Programming Exercise for counting word frequency in a Text

See: http://www.meetup.com/thefunclub/events/104441382/


    wget http://www.textlibrary.com/download/moby-dic.txt
    for I in $(seq 1 100); do cat moby-dic.txt ; done > moby-dic_100.txt


Haskell 2
---------

* Simple solution in Haskell using Data.Map

Haskell 1
---------

* Try to improve the the Haskell 2 - But failed

Bash
----

* Different Bash Solutions

Timing:
------

On intel i5 4x3.3GHz:

    tr ' [A-Z]' '\n[a-z]' < ../moby-dic_100.txt | sed -e 's/[^[:alnum:]]//g' | grep -v ^$ | sort | uniq -c | sort -nr | head
    real  0m13.387s
    user  0m16.600s
    sys 0m1.090s


    tr -cs '[:alnum:]' ' ' < ../moby-dic_100.txt  | tr ' [:upper:]' '\n[:lower:]' | sort | uniq -c | sort -nr | head
    real  0m8.963s
    user  0m9.480s
    sys 0m0.720s

    tr -cs '[:alnum:]' ' ' < ../moby-dic_100.txt  | tr ' [:upper:]' '\n[:lower:]' | sort -S1G --parallel=4 | uniq -c | sort -nr | head
    real  0m6.839s
    user  0m13.960s
    sys 0m1.870s

    tr -cs '[:alnum:]' ' ' < ../moby-dic_100.txt | tr '[:upper:] ' '[:lower:]\n' | awk '{count[]++}END{for(j in count) print count[j],j}' | sort -nr | head
    real  0m5.842s
    user  0m6.450s
    sys 0m0.400s

    tr -cs '[:alnum:]' ' ' < ../moby-dic_100.txt | tr '[:upper:] ' '[:lower:]\n' | awk -f test.awk | sort -nr | head
    real  0m5.779s
    user  0m6.350s
    sys 0m0.420s

Haskell1 (OOM for moby-dic_100.txt):

    $ time ./Solution < ../moby-dic.txt  ## NOT moby-dic_100.txt
    real  0m1.149s
    user  0m1.060s
    sys 0m0.080s

Haskell2:

    $ time ./Solution < ../moby-dic.txt  ## NOT moby-dic_100.txt
    real  0m0.666s
    user  0m0.650s
    sys 0m0.010s
