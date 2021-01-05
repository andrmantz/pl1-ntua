fun halve nil = (nil, nil)  (*Copyrights for halve, merge and MergeSort National Technical University of Athens*)
                            (*http://courses.softlab.ntua.gr/pl1/2019a/Slides/lecture-04.pdf*)    
    | halve [a] = ([a], nil)
    | halve (a::b::cs) =
    let
        val (x, y) = halve cs
    in
        (a::x, b::y)
    end  

fun merge (nil, (ys:int list)) = ys
    | merge ((xs:int list), nil) = xs
    | merge (x::xs, y::ys) =
        if x < y then x :: merge (xs, y::ys)
        else y :: merge (x::xs, ys)


fun mergesort nil = nil 
    | mergesort [a] = [a]
    | mergesort ls = 
        let 
            val (x,y) = halve ls
        in 
            merge (mergesort x, mergesort y)
        end         

    






fun printlist nil = ()
    | printlist (x::[]) = (print(Int.toString(x)); print("\n")  ) (*After the last element, nextline*)
    | printlist (x::xs) = (print(Int.toString(x)); print (" ") ; printlist xs) 

fun printlistofarr A = 
    let
        val z = mergesort(Array.sub(A, 0))
    in
        printlist z
    end
    


(*LookForCycle Algorithm by GEEKSFORGEEKS : https://www.geeksforgeeks.org/print-all-the-cycles-in-an-undirected-graph/*)

fun LookForCycle (current_node:int array) (parent_node : int array) (graph:int list array) (par:int array) (color :int array) (cycle :int list array) :unit = 
    let 
        val c = Array.sub(current_node, 0)
        val c1 = c - 1
        val col = Array.sub(color, c1)
    in 
        if col = 1 then case1 current_node parent_node graph par color cycle 
        else (
            if col = 2 then () 
            else case0 current_node parent_node graph par color cycle 
        )
    end    

and case1 (current_node:int array) (parent_node : int array) (graph:int list array) (par:int array) (color :int array) (cycle :int list array) :unit = 
(  Array.update(cycle, 0, Array.sub(parent_node, 0)::Array.sub(cycle, 0));
    let 
        val c = Array.sub(current_node, 0)
        val p = Array.sub(parent_node, 0)
        val curArr = Array.array(1, p) 
        fun whileloop (cur:int) = 
            if (cur<>Array.sub(current_node, 0)) 
            then (
                (*print("mpika sto 1\n");*)
                Array.update(curArr, 0, Array.sub(par, cur-1));
                Array.update(cycle,0, (Array.sub(curArr,0)::Array.sub(cycle,0)));
                whileloop (Array.sub(curArr, 0))
            )
            else ()
    in 
        whileloop p
    end            
)

and case0 (current_node:int array) (parent_node : int array) (graph:int list array) (par:int array) (color :int array) (cycle :int list array) :unit =
(    let 
        val c = Array.sub(current_node, 0)
        val c1 = c-1
        val z = Array.sub(graph, c1)
        val p = Array.sub(parent_node, 0)
    in

        Array.update(par, c1, p);
        Array.update(color, c1, 1);
                let 

                    fun looplist nil = ()
                        | looplist (x::xs) :unit = (
                            if (x = Array.sub(par, c1)) then (looplist xs)
                            else (
                                Array.update(current_node, 0, x);
                                Array.update(parent_node, 0, c);
                                LookForCycle current_node parent_node graph par color cycle;
                                looplist xs
                            )
                          )
                in 
                    looplist z

                end;
    Array.update(color,c1, 2)                    
    end
)






fun add_edge (node1:int) (node2:int) (graph: int list array) :unit= 
    let
        val x1 = node1 - 1;
        val x2 = node2 - 1;
        val y1 = Array.sub(graph, x1);
        val y2 = Array.sub(graph, x2);
    in
        Array.update(graph, x1, node2::y1);
        Array.update(graph, x2, node1::y2)
    end





fun countnodes (counter:int array) (root:int) (is_visited:bool array) (graph: int list array) =
(
    Array.update(is_visited, root -1, true);
    Array.update(counter,0, Array.sub(counter, 0) + 1);
    let 
        fun forloop [] = ()
            | forloop (x::xs) = (
                if ((Array.sub(is_visited, x -1)) = false)
                then (
                    countnodes counter x is_visited graph;
                    forloop xs
                )  
                else forloop xs
            )
    in 
        forloop (Array.sub(graph, root-1))
    end        
)    




fun initialize (Array1:bool array) (h:int) = ( 
    let 
        val x = h -1
    in     
        Array.update(Array1, x, true)
    end    
)

fun use_initialize (Arr : bool array) (ls : int list)= (
            if (ls <> []) then(
                let 
                    val h = hd ls
                in 
                    initialize Arr h;
                    use_initialize Arr (tl ls)
                end        
            )
            else ()
        )



fun nocorona (colors : int array) :bool = 
    let 
        fun looparray i = 
            let 
                val x = Array.length(colors) 
            in 
                if (i < x) then (    
                    if (Array.sub(colors, i) = 0) then (false)
                    else looparray (i+1))
                else (true)
            end;        
    in 
        looparray 0
    end            


fun ListLength [] counter1 = counter1 
    | ListLength (x::xs) counter1 = 
        ListLength (xs) (counter1+1)


    



fun coronograph file = 
    let 
        fun readInt input =              (* Input parse code by Stavros Aronis, modified by Nick Korasidis. *)
        Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input)

        (* Open input file. *)
        val inStream = TextIO.openIn file

        (* Read an integer (number of countries) and consume newline. *)
        val T = readInt inStream
        val _ = TextIO.inputLine inStream

        fun runTtimes j = 
            if (j < T )
            then(
              (*  print(Int.toString(T)); print("\n");
                print(Int.toString(j)); print("\n");*)
                let 
                    val N = readInt inStream;
                    val M = readInt inStream;
                    val graph = Array.array(N,[])
                    val cycle = Array.array(1,[])
                    val par = Array.array(N,0)
                    val color = Array.array(N,0)
                    val parent_node = Array.array(1,0)
                    val current_node = Array.array(1,1)
                    val results = Array.array(1, [])
                    val is_visited = Array.array(N,false)
                    val counter = Array.array(1, 0)
        
        
                     fun loopNodesForCount [] = ()
                        | loopNodesForCount (x::xs) =( 
                            Array.update(counter, 0, 0);
                            countnodes counter x is_visited graph;
                            Array.update(results, 0, Array.sub(counter,0)::Array.sub(results, 0));
                            loopNodesForCount xs)    


                in 
                    let 
                        fun loop1 i = 
                            if (i < M) 
                            then (
                                let 
                                    val node1 = readInt inStream
                                    val node2 = readInt inStream
                                in 
                                    add_edge node1 node2 graph;
                                    loop1 (i+1)
                                end        
                            )
                            else ()
                    in 
                        loop1 0
                    end;
                    if (N <> M) then ((print "NO CORONA\n");runTtimes (j+1))
                    else (

                        LookForCycle current_node parent_node graph par color cycle;

                        if ((nocorona color) = false )
                        then (
                            print("NO CORONA\n");
                            runTtimes (j+1)
                        )
                        else (
                            let 
                                val x = Array.sub(cycle, 0)
                                val x1 = ListLength x 0
                            in     
                                print("CORONA "); print(Int.toString(x1));
                                print("\n")
                            end ;  
                            use_initialize is_visited (Array.sub(cycle,0));
                            loopNodesForCount (Array.sub(cycle, 0));
                            printlistofarr results;
                            runTtimes (j + 1)
                        )
                    )
                                    
                end                
            )
            else ()
    in 

        runTtimes 0
    end          