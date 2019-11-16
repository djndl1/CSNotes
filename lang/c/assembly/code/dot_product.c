int A[] = {1,2,3,4,5};
int B[] = {1,2,3,4,5};
int C = 0;

int main(int argc, char *argv[])
{
        for (int i = 0; i < 5; i++) {
                C += A[i] * B[i];
        }
        return C;
}

