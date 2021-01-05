#include <iostream>
#include <cmath> 
#include <fstream>

using namespace std; 


int GetFromText(char *infile, int *x, int *y){ //pairnei to areio eisodou, ton pinaka N kai ton pinaka K ki epistrefei to T
    ifstream insidefile;
    insidefile.open(infile);               
    int T, i;
    if (insidefile.is_open()){
        while (!insidefile.eof()){
            insidefile >> T;        //To proto steixeio einai to T 
                for(i=0; i < T; i++){    
                    insidefile >> x[i];  
                    insidefile >> y[i];
                }
        }
    }
    insidefile.close();  //kleisimo arxeiou
    return T;
}
  
int max_length(int n){          //to megisto dunato mikos tou pinaka eksodou
    int x;
    x = int(log2(n)) + 1;     //h megaliteri dinami pou mporei na periexei einai log2(n), ara to megisto mikos log2(n) + 1
    return x;
}


void print_upto(int *arr, int max_length){   //afairese ta midenika sto telos kai ektupose ti lista
    int temp = 0;
    bool flag = false;
    int i = max_length-1;
    while (i >= 0) {         //trexa ti lista anapoda
        if (arr[i] == 0 ){    //oso to arr[i] einai 0, meiose to i kata ena kai proxora stin epomeni epanalipsi
            i--;
            continue;
        }
        else if (arr[i] != 0){         //an arr[i] !=0, break kai epestrepse mia int stin epomeni thesi
            temp = i + 1;
            flag = true;               
            break;
        }
    }
    
    if (!flag){             //an flag==false, tote ektupose keni lista
        cout << "[]"<<endl;
    }
    else{                   
        cout << "[";
        for (int j =0; j < temp; j++){      //ektupose ti lista
            cout << arr[j];
            if (j != temp - 1) {    //mi baleis komma meta to teleutaio stoixeio
                cout << ",";
            }
        }
        cout <<"]"<< endl;
    }    
}

void FindAllElements(int n, int k/*, int *myarr*/) 
{  
    int sum = k, i, temp = 0;         //!!COPYRIGTHS GEEKSFORGEEKS : https://www.geeksforgeeks.org/represent-n-as-the-sum-of-exactly-k-powers-of-two-set-2/
    int sizeofarr = max_length(n);
    int A[k], myarr[sizeofarr];
    
    fill(A, A + k, 1);                     //arxikopoisi pinakon
    fill(myarr, myarr + sizeofarr, 0); 
    
    for (i = k - 1; i >= 0; --i) { 
     
        while (sum + A[i] <= n) { 
            sum += A[i]; 
            A[i] *= 2; 
        } 
    }  

    if (sum != n) {                    //an sum!=n, tote midenise ti lista
        for (i = 0; i < k; i++ ){
           myarr[i] = 0;
        } 
    } 
    else {
        for (i=0; i < k; i++){        
            if (A[i] == 1) {
                myarr[0] ++;
            }
            else {
                temp = log2(A[i]);
                myarr[temp] ++;
            }
        }
    }

    print_upto(myarr, sizeofarr); 
} 
   
int main(int argc, char **argv) 
{ 
    int K[10], N[10], T, i;
    T = GetFromText(argv[1], N, K); //diabase to arxeio pou dinetai san argument
    if (argc <2 || argc > 3) exit(1);  //an ta arguments den einai sosta, termatismos

    for (i=0; i < T; i++){
        

        FindAllElements(N[i], K[i]);
        
        
    }
  
    return 0; 
}     