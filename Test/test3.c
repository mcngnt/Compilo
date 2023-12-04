int res;

int main()
{
    int x;
    int* px;
    int** ppx;

    ppx = &px;
    px = &x;
    x = 10;
    res = **ppx;
    return 0;
}