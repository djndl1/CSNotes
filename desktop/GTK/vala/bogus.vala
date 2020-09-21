void test() throws FileError
{
    throw new FileError.FAILED("test");
}

delegate void Action();

class bogus : GLib.Object
{
    int a;
    public bogus()
    {
        a = 5;
    }
}
