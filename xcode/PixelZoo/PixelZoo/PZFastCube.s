//
//  PZFastCube.s
//  PixelZoo
//
//  Created by Ian Holmes on 11/20/13.
//  Copyright (c) 2013 Holmesian Software. All rights reserved.
//


#if 0
//   R0: pointer to first byte
//   R1: bytes per row
//   R2: top face color
//   R3: left face color
// [sp]: right face color
.fastCube16:
    push {r4-r12,lr}  // decrements sp by 40
    mov r8, r0  // save r0
    mov r10, r1  // save r1
    mov r12, r3  // save r3
// from this point:
// [sp,#40]: right face color
//    R0-R7: current color
//       R8: pointer to first byte of current row
//       R9: pointer to next byte
//      R10: bytes per row
//      R11: pointer to first byte of row 4 (where left & right faces begin)
//      R12: left face color

    // top face
    mov r0, r2
    mov r1, r0
    mov r3, r0
    mov r4, r0
    mov r5, r0
    mov r6, r0
    // row 0
    add r9, r8, #4*8
    stm r9, {r0-r1}
    // row 1
    add r8, r10
    add r9, r8, #4*5
    stm r9, {r0-r5}
    // row 2
    add r8, r10
    add r9, r8, #4*3
    stm r9!, {r0-r4}
    stm r9, {r0-r4}
    // row 3
    add r8, r10
    add r9, r8, #4*1
    stm r9!, {r0-r6}
    stm r9!, {r0-r6}
    // row 4
    add r11, r8, r10   // r11 is set here
    add r9, r11, #4*1
    stm r9!, {r0-r6}
    stm r9, {r0-r6}
    // row 5
    add r8, r11, r10
    add r9, r8, #4*3
    stm r9!, {r0-r4}
    stm r9, {r0-r4}
    // row 6
    add r8, r10
    add r9, r8, #4*5
    stm r9, {r0-r5}
    // row 7
    add r8, r10
    add r9, r8, #4*8
    stm r9, {r0-r1}

    // left face
    mov r0, r12
    mov r1, r0
    mov r2, r0
    mov r3, r0
    mov r4, r0
    mov r5, r0
    mov r6, r0
    // row 4
    stm r11, {r0}
    // row 5
    add r8, r11, r10
    stm r8, {r0-r2}
    // row 6
    add r8, r10
    stm r8, {r0-r4}
    // row 7
    add r8, r10
    stm r8, {r0-r6}
    // row 8
    add r8, r10
    stm r8, {r0-r6,r12}
    // row 9
    add r8, r10
    stm r8, {r0-r6,r12}
    // row 10
    add r8, r10
    stm r8, {r0-r6,r12}
    // row 11
    add r8, r10
    stm r8, {r0-r6,r12}
    // row 12
    add r8, r10
    add r9, r8, #4*1
    stm r9, {r0-r6}
    // row 13
    add r8, r10
    add r9, r8, #4*3
    stm r9, {r0-r4}
    // row 14
    add r8, r10
    add r9, r8, #4*5
    stm r9, {r0-r2}
    // row 14
    add r8, r10
    add r9, r8, #4*7
    stm r9, {r0}

    // right face
    ldr r0, [sp, #40]
    mov r1, r0
    mov r2, r0
    mov r3, r0
    mov r4, r0
    mov r5, r0
    mov r6, r0
    mov r7, r0
    // row 4
    add r9, r11, #4*15
    stm r9, {r0}
    // row 5
    add r8, r11, r10
    add r9, r8, #4*13
    stm r9, {r0-r2}
    // row 6
    add r8, r10
    add r9, r8, #4*11
    stm r9, {r0-r4}
    // row 7
    add r8, r10
    add r9, r8, #4*9
    stm r9, {r0-r6}
    // row 8
    add r8, r10
    add r9, r8, #4*8
    stm r9, {r0-r7}
    // row 9
    add r9, r10
    stm r9, {r0-r7}
    // row 10
    add r9, r10
    stm r9, {r0-r7}
    // row 11
    add r9, r10
    stm r9, {r0-r7}
    // row 12
    add r9, r10
    stm r9, {r0-r6}
    // row 13
    add r9, r10
    stm r9, {r0-r4}
    // row 14
    add r9, r10
    stm r9, {r0-r2}
    // row 14
    add r9, r10
    stm r9, {r0}

    // return
    pop {r4-r12,pc}
#endif
