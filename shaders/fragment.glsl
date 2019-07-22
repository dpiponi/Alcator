#version 110

// Acorn atom graphics modes
// Mode:       Resolution:          Memory:   #B000
//             X:       Y:
//  0          64       48           0.5 K    #00
//  1a         64       64             1 K    #10
//  1         128       64             1 K    #30
//  2a        128       64             2 K    #50
//  2         128       96           1.5 K    #70
//  3a        128       96             3 K    #90
//  3         128      192             3 K    #B0
//  4a        128      192             6 K    #D0
//  4         256      192             6 K    #F0

// 0 green
// 1 yellow
// 2 blue
// 3 red

float testbit(int byte, int bit) {
  int px = int(pow(2., float(bit)));
  return mod(float(byte), float(2*px)) >= float(px) ? 1.0 : 0.0;
}

vec4 colour(int bits) {
    if (bits == 0) {
        return vec4(0.0, 1.0, 0.0, 1.0);
    } else if (bits == 1) {
        return vec4(1.0, 1.0, 0.0, 1.0);
    } else if (bits == 2) {
        return vec4(0.0, 0.0, 1.0, 1.0);
    } else {
        return vec4(1.0, 0.0, 0.0, 1.0);
    }
}

uniform sampler2D current_frame;
uniform sampler2D table;
uniform sampler2D last_frame;
uniform float mode;
varying vec2 texcoord;

float scanline(float y) {
    float w=0.5*0.5*0.125;
    return w*floor(y)+min(y-floor(y),w);
}

float scanline_aa(float y) {
    float h=1.0*dFdy(191.*y);
    return 1.-(scanline(192.*y+0.5*h)-scanline(192.*y-0.5*h))/h;
}

float scanline_aa_128(float y) {
    float h=1.0*dFdy(128.*y);
    return 1.-(scanline(128.*y+0.5*h)-scanline(128.*y-0.5*h))/h;
}

void main()
{

    if (mode == 0.0) {
//        float h=1.0*dFdy(191.*texcoord.y);
//        float bb = 1.-(scanline(192.*texcoord.y+0.5*h)-scanline(192.*texcoord.y-0.5*h))/h;
        float bb = scanline_aa(texcoord.y);
        int x = int(32.*8.*texcoord.x);
        int y = int(16.*12.*texcoord.y);
        int ix = x/8;
        int iy = y/12;
        int fx = x-8*ix;
        int fy = y-12*iy;
        int addr = 32*iy+ix;
        int ty = addr/128;
        int tx = addr-128*ty;
        vec4 last_index = texture2D(current_frame, vec2(float(tx)/128., float(ty)/128.));
        int character = int(255.0*last_index.x);
        int cy = character/32;
        int cx = character-32*cy;
        int px = 8*cx+fx;
        int py = 12*cy+fy;
        float z = bb*texture2D(table, vec2(float(px)/255.0, float(py)/95.0)).x;
        gl_FragColor = vec4(z, z, z, 1.0);
    } else if (mode == 48.0) {
        float bb = scanline_aa_128(texcoord.y);
        int x = int(128.*texcoord.x);
        int y = int(64.*texcoord.y);
        int ix = x/8;
        int fx = 7-(x-8*ix);
        int addr = 16*y+ix;
        int ty = addr/128;
        int tx = addr-128*ty;
        vec4 last_index = texture2D(current_frame, vec2(float(tx)/128., float(ty)/128.));
        int byte = int(255.0*last_index.x);
        int px = int(pow(2., float(fx)));
        float z = mod(float(byte), float(2*px)) >= float(px) ? 1.0 : 0.0;
        gl_FragColor = bb*vec4(z, z, z, 1.0);
    } else if (mode == 112.0) {
        float bb = scanline_aa(texcoord.y);
        int x = int(128.*texcoord.x);
        int y = int(96.*texcoord.y);
        int ix = x/8;
        int fx = 7-(x-8*ix);
        int addr = 16*y+ix;
        int ty = addr/128;
        int tx = addr-128*ty;
        vec4 last_index = texture2D(current_frame, vec2(float(tx)/128., float(ty)/128.));
        int byte = int(255.0*last_index.x);
        int px = int(pow(2., float(fx)));
        float z = mod(float(byte), float(2*px)) >= float(px) ? 1.0 : 0.0;
        gl_FragColor = bb*vec4(z, z, z, 1.0);
    } else if (mode == 176.0) {
        float bb = scanline_aa(texcoord.y);
        int x = int(128.*texcoord.x);
        int y = int(192.*texcoord.y);
        int ix = x/8;
        int fx = 7-(x-8*ix);
        int addr = 16*y+ix;
        int ty = addr/128;
        int tx = addr-128*ty;
        vec4 last_index = texture2D(current_frame, vec2(float(tx)/128., float(ty)/128.));
        int byte = int(255.0*last_index.x);
        int px = int(pow(2., float(fx)));
        float z = mod(float(byte), float(2*px)) >= float(px) ? 1.0 : 0.0;
        gl_FragColor = bb*vec4(z, z, z, 1.0);
    } else if (mode == 240.0) { // 4
        float bb = scanline_aa(texcoord.y);
        int x = int(256.*texcoord.x);
        int y = int(192.*texcoord.y);
        int ix = x/8;
        int fx = 7-(x-8*ix);
        int addr = 32*y+ix;
        int ty = addr/128;
        int tx = addr-128*ty;
        vec4 last_index = texture2D(current_frame, vec2(float(tx)/128., float(ty)/128.));
        int byte = int(255.0*last_index.x);
        float z = testbit(byte, fx);
        gl_FragColor = bb*vec4(z, z, z, 1.0);
    } else if (mode == 208.0 || true) { // 4a
//        float h=1.0*dFdy(191.*texcoord.y);
//        float bb = 1.-(scanline(192.*texcoord.y+0.5*h)-scanline(192.*texcoord.y-0.5*h))/h;
        float bb = scanline_aa(texcoord.y);
        int x = int(128.*texcoord.x);
        int y = int(192.*texcoord.y);
        int ix = x/4;
        int fx = 3-(x-4*ix);
        int addr = 32*y+ix;
        int ty = addr/128;
        int tx = addr-128*ty;
        vec4 last_index = texture2D(current_frame, vec2(float(tx)/128., float(ty)/128.));
        int byte = int(255.0*last_index.x);
        int px = int(pow(2., float(2*fx)));
        int bits = mod(float(byte), float(2*px)) >= float(px) ? 1 : 0;
        bits += mod(float(byte), float(4*px)) >= float(2*px) ? 2 : 0;
        gl_FragColor = bb*colour(bits);
    }
}

