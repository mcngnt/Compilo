
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
	a = 30;
	while(a > 0)
	{
		print_int_nl(a);
		a--;
	}
	return;
}