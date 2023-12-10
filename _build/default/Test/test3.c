int res;

int print_int(int t)
{
    if(t <= 9)
    {
        putc('0' + t);
        return;
    }
    print_int(t/10);
    putc('0' + (t % 10));
    return;
}

int print_int_nl(int t)
{
    print_int(t);
    putc('\n');
    return;
}

int main()
{
    int x;
    int* px;
    int** ppx;

    ppx = &px;
    px = &x;
    x = 10;
    res = **ppx;
    print_int_nl(res);
    return 0;
}