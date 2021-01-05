#include <bits/stdc++.h> 
#include <fstream>
#include <iostream>
#include <vector>

using namespace std; 


//Global Vectors
vector<int> cycle;
vector<int> colors;
vector<int> par;
vector<bool> is_visited;

// Function to mark the cycle 
void dfs_cycle(int u, int p, vector<int> *graph) //Copyrights GEEKSFORGEEKS : https://www.geeksforgeeks.org/print-all-the-cycles-in-an-undirected-graph/
{ 
  
    // already (completely) visited vertex. 
    if (colors[u-1] == 2) { 
        return; 
    } 
    // seen vertex, but was not completely visited -> cycle detected. 
    // backtrack based on parents to find the complete cycle. 
    if (colors[u-1] == 1) { 
         
        cycle.push_back(p);  //add p in the cycle vector  
  
        // backtrack the vertex which are 
        // in the current cycle thats found 
        while (p != u) { 
            p = par[p-1]; 
            cycle.push_back(p); 
        } 
        return; 
    } 
    par[u-1] = p; 
     
    // partially visited. 
    colors[u-1] = 1; 
  
    // simple dfs on graph 
    for (int v : graph[u-1]) { 
         
        // if it has not been visited previously 
        if (v == par[u-1]) { 
            continue; 
        } 
        dfs_cycle(v, u, graph); 
    } 
  
    // completely visited. 
    colors[u-1] = 2;

} 
  
// add the edges to the graph 
void addEdge(int u, int v, vector<int> *graph) //Copyrights GEEKSFORGEEKS: https://www.geeksforgeeks.org/print-all-the-cycles-in-an-undirected-graph/
{ 
    graph[u-1].push_back(v); 
    graph[v-1].push_back(u); 
} 
  



void countnodes(int &counter, int root, vector<int> *graph){
    is_visited[root-1] = true;   //the root has been visited
    for (int i : graph[root-1]){    
        if (!is_visited[i-1]){   //if node not visited, counter ++ and visit him
            counter ++;
            countnodes(counter ,i, graph );
        }
        
    }
}


int main(int argc, char **argv)
{
    FILE *myfile;
    int T,N, M, counter;
    int node1, node2 ;
    myfile =fopen(argv[1], "r");
    
    fscanf (myfile, "%d",  &T);
     

        for ( int i=0; i < T; i++){
            
            cycle.clear(); par.clear(); 
            colors.clear(); is_visited.clear();
            
            fscanf(myfile, "%u", &N);
            fscanf(myfile, "%u" , &M);

            par.resize(N);  //resize vectors at the size of N
            colors.resize(N);
            is_visited.resize(N);

            vector<int> *graph = new vector<int> [N];  //vector to store the graphs
            vector<int> myresults;
            
            fill(is_visited.begin(), is_visited.end(), false);  //is_visited initialization with false

          
           

            
            for (int j=0; j < M; j++){
                
                fscanf(myfile, "%u", &node1);
                fscanf(myfile, "%u", &node2);
                addEdge(node1, node2, graph);
            }
        

        
            if(N != M) {  //if N!=M, there is no possibility to contain only one cycle
                printf("NO CORONA\n"); //so no corona
                continue;
            }
            
            // call DFS to find the cycle 
            dfs_cycle(1, 0, graph); 
            
            if (count(colors.begin(), colors.end(), 0)){//if there is an unvisited node with color 0, the graph is not connected
                printf("NO CORONA\n");                 //So No corona
                continue;
            }        


            printf("CORONA %zu\n" ,cycle.size());

            for ( int j : cycle) is_visited[j-1] = true; //mark the cycle nodes as visited, so they will not be counted

        
            for (int  j : cycle){//for each node of the cycle, count its tree nodes
                counter = 1;  //initialize counter to 1, as we don't count the root of the tree
                countnodes(counter, j, graph);
                myresults.push_back(counter);
           }
  
            

            sort(myresults.begin(), myresults.end()); 
            for (long unsigned int it =0; it < myresults.size(); it ++){  //sort and print
               if (it != myresults.size()-1){
                   printf("%u ", myresults[it]);
               }
               else printf("%u\n" ,myresults[it]);
           }

           
        }
    
    fclose(myfile);
    return 0;
}   












