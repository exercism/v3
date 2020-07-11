section .text

global expected_minutes_in_oven
expected_minutes_in_oven:
    mov rax, 40
    ret

global remaining_minutes_in_oven
remaining_minutes_in_oven:
    call expected_minutes_in_oven
    sub rax, rdi
    ret

global preparation_time_in_minutes
preparation_time_in_minutes:
    mov rax, rdi
    imul rax, 2
    ret

global elapsed_time_in_minutes
elapsed_time_in_minutes:
    call preparation_time_in_minutes
    add rax, rsi
    ret
