/** This file is a derived work of the parity-wasm project. 
 *
 * `parity-wasm` is primarily distributed under the terms of both the MIT
 * license and the Apache License (Version 2.0), at your choice.
 */
pub fn serialize_i32(x: i32, data: &mut Vec<u8>) {
    let mut buf = [0u8; 1];
    let mut v = x;
    let mut more = true;
    while more {
        buf[0] = (v & 0b0111_1111) as u8;
        v >>= 7;
        if (v == 0 && buf[0] & 0b0100_0000 == 0) || (v == -1 && buf[0] & 0b0100_0000 == 0b0100_0000)  {
            more = false
        } else {
            buf[0] |= 0b1000_0000
        }

        data.push(buf[0]);
    }
}

/// Serialize a canonical LEB128
pub fn serialize_u32(x: u32, data: &mut Vec<u8>) {
    let mut buf: u8;
    let mut v = x;
    loop {
        buf = (v & 0b0111_1111) as u8;
        v >>= 7;
        if v > 0 {
            buf |= 0b1000_0000;
        }
        data.push(buf);
        if v == 0 { break; }
    }
}

/// Serialize a full 5 byte padded LEB128
pub fn serialize_u32_pad(x: u32, data: &mut Vec<u8>) {
    let mut buf: u8;
    let mut num_bytes = 0;
    let mut v = x;
    loop {
        if v == 0 { 
            break; 
        }
        buf = (v & 0b0111_1111) as u8;
        v >>= 7;
        if num_bytes < 5 {
            buf |= 0b1000_0000;
        }
        data.push(buf);
        num_bytes += 1;
    }

    while num_bytes < 4 {
        data.push(0x80);
        num_bytes += 1;
    }

    if num_bytes < 5 {
        data.push(0x00);
    }
}

pub fn get_serialized_size_u32(x: u32) -> u32 {
    let mut out: u32 = 0;
    let mut v = x;
    loop {
        v >>= 7;
        out += 1;
        if v == 0 { break; }
    }
    out
}

pub fn serialize_i64(x: i64, data: &mut Vec<u8>) {
    let mut buf = [0u8; 1];
    let mut v = x;
    let mut more = true;
    while more {
        buf[0] = (v & 0b0111_1111) as u8;
        v >>= 7;
        if (v == 0 && buf[0] & 0b0100_0000 == 0) || (v == -1 && buf[0] & 0b0100_0000 == 0b0100_0000)  {
            more = false
        } else {
            buf[0] |= 0b1000_0000
        }

        data.push(buf[0]);
    }
}

pub fn serialize_f32(x: f32, data: &mut Vec<u8>) {
    let bits = x.to_bits();
    let bytes = bits.to_le_bytes();
    for i in 0..4 {
        data.push(bytes[i]);
    }
}

pub fn serialize_f64(x: f64, data: &mut Vec<u8>) {
    let bits = x.to_bits();
    let bytes = bits.to_le_bytes();
    for i in 0..8 {
        data.push(bytes[i]);
    }
}

pub fn serialize_string(x: &String, data: &mut Vec<u8>) {
    serialize_u32(x.len() as u32, data);
    let mut bytes = x.clone().into_bytes();
    data.append(&mut bytes);
}