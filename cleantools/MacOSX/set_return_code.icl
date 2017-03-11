implementation module set_return_code

set_return_code_world :: !Int !*World -> *World
set_return_code_world i world = code {
    pushI 0
    pushLc return_code
    :xxx
    pop_b 3
|    mov dword ptr [rbx+r10],eax
    instruction 66
    instruction 137
    instruction 4
    instruction 19
    fill_a 0 1
    pop_a 1
}
