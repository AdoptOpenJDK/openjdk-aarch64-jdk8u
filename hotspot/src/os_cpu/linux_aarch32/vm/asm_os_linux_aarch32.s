
.global linux_aarch32_current_frame_pointer
.type linux_aarch32_current_frame_pointer,%function
linux_aarch32_current_frame_pointer:
  mov r0, sp
  bx lr
