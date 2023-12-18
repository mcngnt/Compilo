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
    if(t < 0)
    {
        t = -t;
        putc('-');
    }
    print_int(t);
    putc('\n');
    return;
}

int main()
{
	int a;
    int b;
	a = 30;
    b = a > 10 ? 3 * 10 : 10 % 3;
    print_int_nl(b);
	return;
}