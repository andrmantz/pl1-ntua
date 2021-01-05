(* Input parse code by Stavros Aronis, modified by Nick Korasidis. *)
fun parse file  =
    let
    (* A function to read an integer from specified input. *)
        fun readInt input = 
        Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input)

    (* Open input file. *)
    	val inStream = TextIO.openIn file

        (* Read an integer (number of countries) and consume newline. *)
        val n = readInt inStream
        val _ = TextIO.inputLine inStream
        (* A function to read N integers from the open file. *)
        fun readInts 0  acc = rev acc (* Replace with 'rev acc' for proper order. *)
        | readInts i  acc = readInts (i - 1) (readInt inStream ::  acc)

        val helpArray = Array.fromList(readInts (2*n) [])
    in
        helpArray
    end


(*Theloume kathe fora na emfanizei ton megalitero dunato akeraio pou 
den einai megaluteros tou log2(n). *)
fun log2 n = 
    let 
        fun while1 x counter = (*Ksekinaei apo x=1 kai counter =0 kai diplasiazei to x eos otou ginei megalutero apo to n*)
            if (x <= n) 
                then (
                    if (x < 536870912) then while1 (2*x) (counter + 1) (*536870912 = 2^29. An diplasiastei auto to noumero tha
                                                                        ginei overflow. Dedomenou oti log2(n) <2^30, gia kathe noumero
                                                                        n> 2^29, theloume na epistrefei 29*)
                    else 29
                )
            else (counter - 1)    (*Tha treksei mia epipleon fora, opote theloume to counter-1*)
    in
        while1 1 0
    end            

fun maxsize N =                  (*maxsize tou pinaka einai log2 N + 1*)
    (log2 N) + 1 



fun arraysum Arr =               (*Athroisma stoixeion pinaka*)
    Array.foldl op+ 0 Arr 


fun printarr1 A  =  (*An to teleutaio stoixeio tou pinaka einai !=0, ektupose olon ton pinaka*)  
    let    
        val x = (Array.length A) 
        fun printit i = 
            if i < x then (print(Int.toString(Array.sub(A, i)));
                if i <> (x-1) then (print(","); printit (i + 1))
                else (printit (i+1)))
            else ()
    in 
        printit 0
    end



fun printarr2 A =  (*An to teleutaio stoixeio tou pinaka einai, ektipose mexri to teletautaio mi mediniko stoixeio*)
    let
        val x = (Array.length A) - 1
        fun loopbackwards j =  
                if (Array.sub(A, j)=0 )
                    then (
                        loopbackwards (j-1)
                    )
                else (
                    let 
                        fun printit i = 
                            if (i < j + 1) 
                                then (
                                    if (i <> j) 
                                        then (print(Int.toString(Array.sub(A,i))); print(",");printit (i+1))
                                    else print(Int.toString(Array.sub(A,i)))    
                                )
                            else ()
                    in 
                        printit 0
                    end                
                )    
    in 
        loopbackwards x
    end                



fun printarr A  = 
    let 
        val z = (Array.length A) - 1
        val b = Array.sub(A,z)
    in
        if b <> 0 
            then printarr1 A
        else printarr2 A    
    end



fun sumduplicates A B K N  =
    let 
        
        fun loop i = 
            if (i < K) 
                then ( 
                    let 
                        val y = Array.sub(A, i);
                        val temp = log2 y;
                    in    
                        Array.update(B, temp, Array.sub(B, temp) + 1); loop (i+1)
                    end    )
            else (printarr B)        
    in 
        loop 0
    end



(*Algorith copyrigths : https://www.geeksforgeeks.org/represent-n-as-the-sum-of-exactly-k-powers-of-two-set-2/    *)
fun Findallelements A B K N =   
    let
        val sum = Array.array(1,K)
        fun makeA i =  
                if i < 0 then ()
                else (
                    if (Array.sub(sum, 0) <= N - Array.sub(A,i)) 
                    then(
                        Array.update(sum, 0, Array.sub(sum, 0) + Array.sub(A,i));
                       (* print(Int.toString(Array.sub(sum, 0))); print("\n");*)
                        Array.update(A, i, 2*Array.sub(A,i));
                        makeA i
                    )
                    else (
                        makeA (i-1)
                    )
                )
    in 
        makeA (K-1);
        if (arraysum A <> N) then print "[]\n"
        else (
            print("[");
            sumduplicates A B K N;
            print("]\n")
        )
    end  



fun powers2 file =
    let 
        val KNarr = parse file
        val T = (Array.length KNarr) div 2
        fun loopTtimes j =  
                    if j < (2*T) - 1  
                        then (
                            let 
                                val N = Array.sub(KNarr, j)
                                val K = Array.sub(KNarr, j+1)
                                val x = maxsize N
                                val A = Array.array(K, 1)
                                val B = Array.array(x, 0)
                            in 
                                Findallelements A B K N;
                                loopTtimes (j + 2) 
                            end        
                        )
                    else ()    
    in 
        (*print(Int.toString(T));print("\n")*)
        loopTtimes 0
    end    