/*
export function addd(x: number, y: number, z: number): number {
    let a = 1;
    return x + y + z + a;
}
*/


export function negMax(x: number, y: number): number {
    let b = true;
    if(b && x > y)
        return -x;
    else {
        return -y;
    }
    // today, wasm requires a return value at the end of all functions, even if they will
    // never get there
    return 0;
}

/*
export function mul3(x: number, y: number, z: number): number {
    return x * y * z;
}
*/