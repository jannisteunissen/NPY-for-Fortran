program main
    use iso_fortran_env
    use m_npy

    integer     :: i,j
    integer(int8)  :: i1(10,11)
    integer(int16)  :: i2(10,11)
    integer(int32)  :: i4(10,11)
    integer(int64)  :: i8(10,11)
    real(real32)     :: r4(10,11)
    real(real64)     :: r8(10,11)
    complex(4)  :: c4(10,11)
    complex(8)  :: c8(10,11)

    do i=1,10
      do j=1,11
         i1(i,j) = int(10*i+j, int8)
         i2(i,j) = int(10*i+j, int16)
         i4(i,j) = int(10*i+j, int32)
         i8(i,j) = int(10*i+j, int64)
         r4(i,j) = 1000*i + j
         r8(i,j) = 1000*i + j
         c4(i,j) = cmplx(i,j)
         c8(i,j) = cmplx(i,j)
      enddo
   enddo

   ! Add to zip and remove temporary files
   call remove_file("example_1.npz")
   call add_npz("example_1.npz", "i1", i1)
   call add_npz("example_1.npz", "i2", i2)
   call add_npz("example_1.npz", "i4", i4)
   call add_npz("example_1.npz", "i8", i8)
   call add_npz("example_1.npz", "c8", c8)
   call add_npz("example_1.npz", "c4", c4)
   call add_npz("example_1.npz", "r8", r8)
   call add_npz("example_1.npz", "r4", r4)

   call save_npy("i1.npy", i1)
   call save_npy("i2.npy", i2)
   call save_npy("i4.npy", i4)
   call save_npy("i8.npy", i8)
   call save_npy("c8.npy", c8)
   call save_npy("c4.npy", c4)
   call save_npy("r8.npy", r8)
   call save_npy("r4.npy", r4)

   ! Add to zip and keep original files
   call remove_file("example_2.npz")
   call add_to_zip("example_2.npz", "i1.npy", .true.)
   call add_to_zip("example_2.npz", "i2.npy", .true.)
   call add_to_zip("example_2.npz", "i4.npy", .true.)
   call add_to_zip("example_2.npz", "i8.npy", .true.)
   call add_to_zip("example_2.npz", "c8.npy", .true.)
   call add_to_zip("example_2.npz", "c4.npy", .true.)
   call add_to_zip("example_2.npz", "r8.npy", .true.)
   call add_to_zip("example_2.npz", "r4.npy", .true.)

end program main
