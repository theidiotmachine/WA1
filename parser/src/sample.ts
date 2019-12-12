/**
 * Generic function
 * @param b 
 */
function a<S>(b: S): S{
    return b;
}

a<number>(5);

class B<U> {
    m: number;
}