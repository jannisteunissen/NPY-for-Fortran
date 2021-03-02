module m_npy
  use iso_fortran_env

  implicit none
  private

  ! Magic number hex x93 is 147 (unsigned), signed this is -109
  integer(int8), parameter    :: magic_num = int(-109, int8)
  integer(int8), parameter    :: major = 2_int8 ! major *.npy version
  integer(int8), parameter    :: minor = 0_int8 ! minor *.npy version
  character(len=*), parameter :: zip_flag  = "-q0"
  character(len=*), parameter :: magic_str = "NUMPY"

  interface save_npy
    module procedure write_int64_vec, write_int64_mtx, &
         write_int32_vec, write_int32_mtx, write_int32_3d, &
         write_int16_vec, write_int16_mtx, &
         write_int8_vec, write_int8_mtx, write_int8_3d, &
         write_dbl_vec, write_dbl_mtx, &
         write_sng_vec, write_sng_mtx, &
         write_cmplx_sgn_vec, write_cmplx_sgn_mtx, &
         write_cmplx_dbl_vec, write_cmplx_dbl_mtx, &
         write_sng_3dT, write_dbl_3dT, &
         write_sng_4dT, write_dbl_4dT, &
         write_dbl_5dT, &
         write_cmplx_dbl_3dT, &
         write_cmplx_dbl_4dT, &
         write_cmplx_dbl_5dT, &
         write_cmplx_dbl_6dT
  end interface save_npy

  interface add_npz
    module procedure addrpl_int8_vec, addrpl_int8_mtx, &
         addrpl_int16_vec, addrpl_int16_mtx, &
         addrpl_int32_vec, addrpl_int32_mtx, &
         addrpl_int64_vec, addrpl_int64_mtx, &
         addrpl_sng_vec, addrpl_sng_mtx, &
         addrpl_dbl_vec, addrpl_dbl_mtx, &
         addrpl_cmplx_dbl_vec, addrpl_cmplx_dbl_mtx, &
         addrpl_cmplx_sng_vec, addrpl_cmplx_sng_mtx
  end interface add_npz

  public :: save_npy
  public :: add_npz

contains

  subroutine run_sys(cmd, stat)
    character(len=*), intent(in) :: cmd
    integer(int32), intent(out)  :: stat

    call execute_command_line(cmd, wait=.True., exitstat=stat)
  end subroutine run_sys

  subroutine convert_to_zip(zipfile, npy_name)
    character(len=*), intent(in) :: zipfile, npy_name
    integer(int32)               :: succ

    ! just store and be quite while zipping
    call run_sys("zip "//zip_flag//" "//zipfile &
         //" "//npy_name, succ)
    if (succ /= 0) then
      write (*, *) "Can't execute zip command"
    endif

    call run_sys("rm "//npy_name, succ)
    if (succ /= 0) then
      write (*, *) "Can't execute rm command"
    endif
  end subroutine convert_to_zip

  subroutine addrpl_cmplx_sng_vec(zipfile, var_name, vec)
    complex(4), intent(in)           :: vec(:)
    character(len=*), intent(in)     :: zipfile, var_name
    call save_npy(var_name//".npy", vec)
    call convert_to_zip(zipfile, var_name//".npy")
  end subroutine addrpl_cmplx_sng_vec

  subroutine addrpl_cmplx_sng_mtx(zipfile, var_name, mtx)
    complex(4), intent(in)       :: mtx(:, :)
    character(len=*), intent(in) :: zipfile, var_name
    call save_npy(var_name//".npy", mtx)
    call convert_to_zip(zipfile, var_name//".npy")
  end subroutine addrpl_cmplx_sng_mtx

  subroutine addrpl_cmplx_dbl_vec(zipfile, var_name, vec)
    complex(8), intent(in)           :: vec(:)
    character(len=*), intent(in)     :: zipfile, var_name
    call save_npy(var_name//".npy", vec)
    call convert_to_zip(zipfile, var_name//".npy")
  end subroutine addrpl_cmplx_dbl_vec

  subroutine addrpl_cmplx_dbl_mtx(zipfile, var_name, mtx)
    complex(8), intent(in)           :: mtx(:, :)
    character(len=*), intent(in)     :: zipfile, var_name
    call save_npy(var_name//".npy", mtx)
    call convert_to_zip(zipfile, var_name//".npy")
  end subroutine addrpl_cmplx_dbl_mtx

  subroutine addrpl_dbl_vec(zipfile, var_name, vec)
    real(real64), intent(in)           :: vec(:)
    character(len=*), intent(in)     :: zipfile, var_name
    call save_npy(var_name//".npy", vec)
    call convert_to_zip(zipfile, var_name//".npy")
  end subroutine addrpl_dbl_vec

  subroutine addrpl_dbl_mtx(zipfile, var_name, mtx)
    real(real64), intent(in)           :: mtx(:, :)
    character(len=*), intent(in)     :: zipfile, var_name
    call save_npy(var_name//".npy", mtx)
    call convert_to_zip(zipfile, var_name//".npy")
  end subroutine addrpl_dbl_mtx

  subroutine addrpl_sng_vec(zipfile, var_name, vec)
    real(real32), intent(in)           :: vec(:)
    character(len=*), intent(in)     :: zipfile, var_name
    call save_npy(var_name//".npy", vec)
    call convert_to_zip(zipfile, var_name//".npy")
  end subroutine addrpl_sng_vec

  subroutine addrpl_sng_mtx(zipfile, var_name, mtx)
    real(real32), intent(in)           :: mtx(:, :)
    character(len=*), intent(in)     :: zipfile, var_name
    call save_npy(var_name//".npy", mtx)
    call convert_to_zip(zipfile, var_name//".npy")
  end subroutine addrpl_sng_mtx

  subroutine addrpl_int8_vec(zipfile, var_name, vec)
    integer(int8), intent(in)           :: vec(:)
    character(len=*), intent(in)     :: zipfile, var_name
    call save_npy(var_name//".npy", vec)
    call convert_to_zip(zipfile, var_name//".npy")
  end subroutine addrpl_int8_vec

  subroutine addrpl_int8_mtx(zipfile, var_name, mtx)
    integer(int8), intent(in)           :: mtx(:, :)
    character(len=*), intent(in)     :: zipfile, var_name
    call save_npy(var_name//".npy", mtx)
    call convert_to_zip(zipfile, var_name//".npy")
  end subroutine addrpl_int8_mtx

  subroutine addrpl_int16_vec(zipfile, var_name, vec)
    integer(int16), intent(in)           :: vec(:)
    character(len=*), intent(in)     :: zipfile, var_name
    call save_npy(var_name//".npy", vec)
    call convert_to_zip(zipfile, var_name//".npy")
  end subroutine addrpl_int16_vec

  subroutine addrpl_int16_mtx(zipfile, var_name, mtx)
    integer(int16), intent(in)           :: mtx(:, :)
    character(len=*), intent(in)     :: zipfile, var_name
    call save_npy(var_name//".npy", mtx)
    call convert_to_zip(zipfile, var_name//".npy")
  end subroutine addrpl_int16_mtx

  subroutine addrpl_int32_vec(zipfile, var_name, vec)
    integer(int32), intent(in)           :: vec(:)
    character(len=*), intent(in)     :: zipfile, var_name
    call save_npy(var_name//".npy", vec)
    call convert_to_zip(zipfile, var_name//".npy")
  end subroutine addrpl_int32_vec

  subroutine addrpl_int32_mtx(zipfile, var_name, mtx)
    integer(int32), intent(in)           :: mtx(:, :)
    character(len=*), intent(in)     :: zipfile, var_name
    call save_npy(var_name//".npy", mtx)
    call convert_to_zip(zipfile, var_name//".npy")
  end subroutine addrpl_int32_mtx

  subroutine addrpl_int64_vec(zipfile, var_name, vec)
    integer(int64), intent(in)           :: vec(:)
    character(len=*), intent(in)     :: zipfile, var_name
    call save_npy(var_name//".npy", vec)
    call convert_to_zip(zipfile, var_name//".npy")
  end subroutine addrpl_int64_vec

  subroutine addrpl_int64_mtx(zipfile, var_name, mtx)
    integer(int64), intent(in)           :: mtx(:, :)
    character(len=*), intent(in)     :: zipfile, var_name
    call save_npy(var_name//".npy", mtx)
    call convert_to_zip(zipfile, var_name//".npy")
  end subroutine addrpl_int64_mtx

  Subroutine write_cmplx_sgn_mtx(filename, mtx)
    character(len=*), intent(in) :: filename
    complex(4), intent(in)       :: mtx(:, :)
    character(len=*), parameter  :: var_type = "<c8"
    integer(int32)               :: header_len, s_mtx(2), p_un

    s_mtx = shape(mtx)
    header_len = len(dict_str(var_type, s_mtx))

    open (newunit=p_un, file=filename, form="unformatted", &
         access="stream")
    write (p_un) magic_num, magic_str, major, minor
    write (p_un) header_len
    write (p_un) dict_str(var_type, s_mtx)

    write (p_un) mtx

    close (unit=p_un)
  End Subroutine write_cmplx_sgn_mtx

  Subroutine write_cmplx_sgn_vec(filename, vec)
    character(len=*), intent(in)     :: filename
    complex(4), intent(in)           :: vec(:)
    character(len=*), parameter      :: var_type = "<c8"
    integer(int32)                       :: header_len, s_vec(1), p_un

    s_vec = shape(vec)
    header_len = len(dict_str(var_type, s_vec))

    open (newunit=p_un, file=filename, form="unformatted", &
         access="stream")
    write (p_un) magic_num, magic_str, major, minor
    write (p_un) header_len

    write (p_un) dict_str(var_type, s_vec)

    write (p_un) vec

    close (unit=p_un)
  End Subroutine write_cmplx_sgn_vec

  Subroutine write_cmplx_dbl_6dT(filename, tensor)
    character(len=*), intent(in)     :: filename
    complex(8), intent(in)           :: tensor(:, :, :, :, :, :)
    character(len=*), parameter      :: var_type = "<c16"
    integer(int32)                       :: header_len, p_un

    header_len = len(dict_str(var_type, shape(tensor)))

    open (newunit=p_un, file=filename, form="unformatted", &
         access="stream")
    write (p_un) magic_num, magic_str, major, minor

    write (p_un) header_len

    write (p_un) dict_str(var_type, shape(tensor))
    write (p_un) tensor
    close (unit=p_un)
  End Subroutine write_cmplx_dbl_6dT

  Subroutine write_cmplx_dbl_5dT(filename, tensor)
    character(len=*), intent(in)     :: filename
    complex(8), intent(in)           :: tensor(:, :, :, :, :)
    character(len=*), parameter      :: var_type = "<c16"
    integer(int32)                       :: header_len, p_un

    header_len = len(dict_str(var_type, shape(tensor)))

    open (newunit=p_un, file=filename, form="unformatted", &
         access="stream")
    write (p_un) magic_num, magic_str, major, minor

    write (p_un) header_len

    write (p_un) dict_str(var_type, shape(tensor))
    write (p_un) tensor
    close (unit=p_un)
  End Subroutine write_cmplx_dbl_5dT

  Subroutine write_cmplx_dbl_4dT(filename, tensor)
    character(len=*), intent(in)     :: filename
    complex(8), intent(in)           :: tensor(:, :, :, :)
    character(len=*), parameter      :: var_type = "<c16"
    integer(int32)                       :: header_len, p_un

    header_len = len(dict_str(var_type, shape(tensor)))

    open (newunit=p_un, file=filename, form="unformatted", &
         access="stream")
    write (p_un) magic_num, magic_str, major, minor

    write (p_un) header_len

    write (p_un) dict_str(var_type, shape(tensor))
    write (p_un) tensor
    close (unit=p_un)
  End Subroutine write_cmplx_dbl_4dT

  Subroutine write_cmplx_dbl_3dT(filename, tensor)
    character(len=*), intent(in) :: filename
    complex(8), intent(in)       :: tensor(:, :, :)
    character(len=*), parameter  :: var_type = "<c16"
    integer(int32)               :: header_len, p_un

    header_len = len(dict_str(var_type, shape(tensor)))

    open (newunit=p_un, file=filename, form="unformatted", &
         access="stream")
    write (p_un) magic_num, magic_str, major, minor

    write (p_un) header_len

    write (p_un) dict_str(var_type, shape(tensor))
    write (p_un) tensor
    close (unit=p_un)
  End Subroutine write_cmplx_dbl_3dT

  Subroutine write_cmplx_dbl_mtx(filename, mtx)
    character(len=*), intent(in)     :: filename
    complex(8), intent(in)           :: mtx(:, :)
    character(len=*), parameter      :: var_type = "<c16"
    integer(int32)                       :: header_len, s_mtx(2), p_un

    s_mtx = shape(mtx)
    header_len = len(dict_str(var_type, s_mtx))

    open (newunit=p_un, file=filename, form="unformatted", &
         access="stream")
    write (p_un) magic_num, magic_str, major, minor

    write (p_un) header_len

    write (p_un) dict_str(var_type, s_mtx)

    write (p_un) mtx

    close (unit=p_un)
  End Subroutine write_cmplx_dbl_mtx

  Subroutine write_cmplx_dbl_vec(filename, vec)
    character(len=*), intent(in)     :: filename
    complex(8), intent(in)           :: vec(:)
    character(len=*), parameter      :: var_type = "<c16"
    integer(int32)                       :: header_len, s_vec(1), p_un

    s_vec = shape(vec)
    header_len = len(dict_str(var_type, s_vec))

    open (newunit=p_un, file=filename, form="unformatted", &
         access="stream")
    write (p_un) magic_num, magic_str, major, minor

    write (p_un) header_len

    write (p_un) dict_str(var_type, s_vec)

    write (p_un) vec

    close (unit=p_un)
  End Subroutine write_cmplx_dbl_vec

  Subroutine write_sng_3dT(filename, tensor)
    character(len=*), intent(in)     :: filename
    real(real32), intent(in)              :: tensor(:, :, :)
    character(len=*), parameter      :: var_type = "<f4"
    integer(int32)                       :: header_len, p_un

    header_len = len(dict_str(var_type, shape(tensor)))

    open (newunit=p_un, file=filename, form="unformatted", &
         access="stream")
    write (p_un) magic_num, magic_str, major, minor

    write (p_un) header_len

    write (p_un) dict_str(var_type, shape(tensor))
    write (p_un) tensor
    close (unit=p_un)
  End Subroutine write_sng_3dT

  Subroutine write_sng_4dT(filename, tensor)
    character(len=*), intent(in)     :: filename
    real(real32), intent(in)              :: tensor(:, :, :, :)
    character(len=*), parameter      :: var_type = "<f4"
    integer(int32)                       :: header_len, p_un

    header_len = len(dict_str(var_type, shape(tensor)))

    open (newunit=p_un, file=filename, form="unformatted", &
         access="stream")
    write (p_un) magic_num, magic_str, major, minor

    write (p_un) header_len

    write (p_un) dict_str(var_type, shape(tensor))
    write (p_un) tensor
    close (unit=p_un)
  End Subroutine write_sng_4dT

  Subroutine write_sng_mtx(filename, mtx)
    character(len=*), intent(in)     :: filename
    real(real32), intent(in)              :: mtx(:, :)
    character(len=*), parameter      :: var_type = "<f4"
    integer(int32)                       :: header_len, s_mtx(2), p_un

    s_mtx = shape(mtx)
    header_len = len(dict_str(var_type, s_mtx))

    open (newunit=p_un, file=filename, form="unformatted", &
         access="stream")
    write (p_un) magic_num, magic_str, major, minor

    write (p_un) header_len

    write (p_un) dict_str(var_type, s_mtx)

    write (p_un) mtx

    close (unit=p_un)
  End Subroutine write_sng_mtx

  Subroutine write_sng_vec(filename, vec)
    character(len=*), intent(in) :: filename
    real(real32), intent(in)          :: vec(:)
    character(len=*), parameter  :: var_type = "<f4"
    integer(int32)               :: header_len, s_vec(1), p_un

    s_vec = shape(vec)
    header_len = len(dict_str(var_type, s_vec))

    open (newunit=p_un, file=filename, form="unformatted", &
         access="stream")
    write (p_un) magic_num, magic_str, major, minor

    write (p_un) header_len

    write (p_un) dict_str(var_type, s_vec)

    write (p_un) vec

    close (unit=p_un)
  End Subroutine write_sng_vec

  Subroutine write_dbl_3dT(filename, tensor)
    character(len=*), intent(in)     :: filename
    real(real64), intent(in)              :: tensor(:, :, :)
    character(len=*), parameter      :: var_type = "<f8"
    integer(int32)                       :: header_len, p_un

    header_len = len(dict_str(var_type, shape(tensor)))

    open (newunit=p_un, file=filename, form="unformatted", &
         access="stream")
    write (p_un) magic_num, magic_str, major, minor

    write (p_un) header_len

    write (p_un) dict_str(var_type, shape(tensor))
    write (p_un) tensor
    close (unit=p_un)
  End Subroutine write_dbl_3dT

  Subroutine write_dbl_4dT(filename, tensor4)
    character(len=*), intent(in)     :: filename
    real(real64), intent(in)              :: tensor4(:, :, :, :)
    character(len=*), parameter      :: var_type = "<f8"
    integer(int32)                       :: header_len, p_un

    header_len = len(dict_str(var_type, shape(tensor4)))

    open (newunit=p_un, file=filename, form="unformatted", &
         access="stream")
    write (p_un) magic_num, magic_str, major, minor

    write (p_un) header_len

    write (p_un) dict_str(var_type, shape(tensor4))
    write (p_un) tensor4
    close (unit=p_un)
  End Subroutine write_dbl_4dT

  Subroutine write_dbl_5dT(filename, tensor5)
    character(len=*), intent(in)     :: filename
    real(real64), intent(in)              :: tensor5(:, :, :, :, :)
    character(len=*), parameter      :: var_type = "<f8"
    integer(int32)                       :: header_len, p_un

    header_len = len(dict_str(var_type, shape(tensor5)))

    open (newunit=p_un, file=filename, form="unformatted", &
         access="stream")
    write (p_un) magic_num, magic_str, major, minor

    write (p_un) header_len

    write (p_un) dict_str(var_type, shape(tensor5))
    write (p_un) tensor5
    close (unit=p_un)
  End Subroutine write_dbl_5dT

  Subroutine write_dbl_mtx(filename, mtx)
    character(len=*), intent(in)     :: filename
    real(real64), intent(in)              :: mtx(:, :)
    character(len=*), parameter      :: var_type = "<f8"
    integer(int32)                       :: header_len, s_mtx(2), p_un

    s_mtx = shape(mtx)
    header_len = len(dict_str(var_type, s_mtx))

    open (newunit=p_un, file=filename, form="unformatted", &
         access="stream")
    write (p_un) magic_num, magic_str, major, minor

    write (p_un) header_len

    write (p_un) dict_str(var_type, s_mtx)

    write (p_un) mtx

    close (unit=p_un)
  End Subroutine write_dbl_mtx

  Subroutine write_dbl_vec(filename, vec)
    character(len=*), intent(in)     :: filename
    real(real64), intent(in)              :: vec(:)
    character(len=*), parameter      :: var_type = "<f8"
    integer(int32)                       :: header_len, s_vec(1), p_un

    s_vec = shape(vec)
    header_len = len(dict_str(var_type, s_vec))

    open (newunit=p_un, file=filename, form="unformatted", &
         access="stream")
    write (p_un) magic_num, magic_str, major, minor

    write (p_un) header_len

    write (p_un) dict_str(var_type, s_vec)

    write (p_un) vec

    close (unit=p_un)
  End Subroutine write_dbl_vec

  Subroutine write_int64_mtx(filename, mtx)
    character(len=*), intent(in)     :: filename
    integer(int64), intent(in)           :: mtx(:, :)
    character(len=*), parameter      :: var_type = "<i8"
    integer(int32)                       :: header_len, s_mtx(2), p_un

    s_mtx = shape(mtx)
    header_len = len(dict_str(var_type, s_mtx))

    open (newunit=p_un, file=filename, form="unformatted", &
         access="stream")
    write (p_un) magic_num, magic_str, major, minor

    write (p_un) header_len

    write (p_un) dict_str(var_type, s_mtx)

    write (p_un) mtx

    close (unit=p_un)
  End Subroutine write_int64_mtx

  Subroutine write_int64_vec(filename, vec)
    character(len=*), intent(in)     :: filename
    integer(int64), intent(in)           :: vec(:)
    character(len=*), parameter      :: var_type = "<i8"
    integer(int32)                       :: header_len, s_vec(1), p_un

    s_vec = shape(vec)
    header_len = len(dict_str(var_type, s_vec))

    open (newunit=p_un, file=filename, form="unformatted", &
         access="stream")
    write (p_un) magic_num, magic_str, major, minor

    write (p_un) header_len

    write (p_un) dict_str(var_type, s_vec)

    write (p_un) vec

    close (unit=p_un)
  End Subroutine write_int64_vec

  Subroutine write_int32_mtx(filename, mtx)
    character(len=*), intent(in)     :: filename
    integer(int32), intent(in)           :: mtx(:, :)
    character(len=*), parameter      :: var_type = "<i4"
    integer(int32)                       :: header_len, s_mtx(2), p_un

    s_mtx = shape(mtx)
    header_len = len(dict_str(var_type, s_mtx))

    open (newunit=p_un, file=filename, form="unformatted", &
         access="stream")
    write (p_un) magic_num, magic_str, major, minor

    write (p_un) header_len

    write (p_un) dict_str(var_type, s_mtx)

    write (p_un) mtx

    close (unit=p_un)
  End Subroutine write_int32_mtx

  Subroutine write_int32_3d(filename, mtx)
    character(len=*), intent(in)     :: filename
    integer(int32), intent(in)           :: mtx(:,:,:)
    character(len=*), parameter      :: var_type = "<i4"
    integer(int32)                       :: header_len, s_mtx(3), p_un

    s_mtx = shape(mtx)
    header_len = len(dict_str(var_type, s_mtx))

    open (newunit=p_un, file=filename, form="unformatted", &
         access="stream")
    write (p_un) magic_num, magic_str, major, minor

    write (p_un) header_len

    write (p_un) dict_str(var_type, s_mtx)

    write (p_un) mtx

    close (unit=p_un)
  End Subroutine write_int32_3d

  Subroutine write_int32_vec(filename, vec)
    character(len=*), intent(in)     :: filename
    integer(int32), intent(in)           :: vec(:)
    character(len=*), parameter      :: var_type = "<i4"
    integer(int32)                       :: header_len, s_vec(1), p_un

    s_vec = shape(vec)
    header_len = len(dict_str(var_type, s_vec))

    open (newunit=p_un, file=filename, form="unformatted", &
         access="stream")
    write (p_un) magic_num, magic_str, major, minor

    write (p_un) header_len

    write (p_un) dict_str(var_type, s_vec)

    write (p_un) vec

    close (unit=p_un)
  End Subroutine write_int32_vec

  Subroutine write_int16_mtx(filename, mtx)
    character(len=*), intent(in)     :: filename
    integer(int16), intent(in)           :: mtx(:, :)
    character(len=*), parameter      :: var_type = "<i2"
    integer(int32)                       :: header_len, s_mtx(2), p_un

    s_mtx = shape(mtx)
    header_len = len(dict_str(var_type, s_mtx))

    open (newunit=p_un, file=filename, form="unformatted", &
         access="stream")
    write (p_un) magic_num, magic_str, major, minor

    write (p_un) header_len

    write (p_un) dict_str(var_type, s_mtx)

    write (p_un) mtx

    close (unit=p_un)
  End Subroutine write_int16_mtx

  Subroutine write_int16_vec(filename, vec)
    character(len=*), intent(in)     :: filename
    integer(int16), intent(in)           :: vec(:)
    character(len=*), parameter      :: var_type = "<i2"
    integer(int32)                       :: header_len, s_vec(1), p_un

    s_vec = shape(vec)
    header_len = len(dict_str(var_type, s_vec))

    open (newunit=p_un, file=filename, form="unformatted", &
         access="stream")
    write (p_un) magic_num, magic_str, major, minor

    write (p_un) header_len

    write (p_un) dict_str(var_type, s_vec)

    write (p_un) vec

    close (unit=p_un)
  End Subroutine write_int16_vec

  Subroutine write_int8_mtx(filename, mtx)
    character(len=*), intent(in)     :: filename
    integer(int8), intent(in)           :: mtx(:, :)
    character(len=*), parameter      :: var_type = "<i1"
    integer(int32)                       :: header_len, s_mtx(2), p_un

    s_mtx = shape(mtx)
    header_len = len(dict_str(var_type, s_mtx))

    open (newunit=p_un, file=filename, form="unformatted", &
         access="stream")
    write (p_un) magic_num, magic_str, major, minor

    write (p_un) header_len

    write (p_un) dict_str(var_type, s_mtx)

    write (p_un) mtx

    close (unit=p_un)
  End Subroutine write_int8_mtx

  Subroutine write_int8_3d(filename, mtx)
    character(len=*), intent(in)     :: filename
    integer(int8), intent(in)           :: mtx(:,:,:)
    character(len=*), parameter      :: var_type = "<i1"
    integer(int32)                       :: header_len, s_mtx(3), p_un

    s_mtx = shape(mtx)
    header_len = len(dict_str(var_type, s_mtx))

    open (newunit=p_un, file=filename, form="unformatted", &
         access="stream")
    write (p_un) magic_num, magic_str, major, minor

    write (p_un) header_len

    write (p_un) dict_str(var_type, s_mtx)

    write (p_un) mtx

    close (unit=p_un)
  End Subroutine write_int8_3d

  Subroutine write_int8_vec(filename, vec)
    character(len=*), intent(in) :: filename
    integer(int8), intent(in)    :: vec(:)
    character(len=*), parameter  :: var_type = "<i1"
    integer(int32)               :: header_len, s_vec(1), p_un

    s_vec = shape(vec)
    header_len = len(dict_str(var_type, s_vec))

    open (newunit=p_un, file=filename, form="unformatted", &
         access="stream")
    write (p_un) magic_num, magic_str, major, minor

    write (p_un) header_len

    write (p_un) dict_str(var_type, s_vec)

    write (p_un) vec

    close (unit=p_un)
  End Subroutine write_int8_vec

  function dict_str(var_type, var_shape) result(str)
    character(len=*), intent(in)   :: var_type
    integer(int32), intent(in)         :: var_shape(:)
    character(len=:), allocatable  :: str
    integer(int32)                     :: cnt

    cnt = len("{'descr': '")
    cnt = cnt + len(var_type)
    cnt = cnt + len("', 'fortran_order': True, 'shape': (")
    cnt = cnt + len(shape_str(var_shape))
    cnt = cnt + len(",), }")
    do while (mod(cnt + 10, 16) /= 0)
      cnt = cnt + 1
    enddo

    allocate (character(cnt) :: str)

    str = "{'descr': '"//var_type// &
         "', 'fortran_order': True, 'shape': ("// &
         shape_str(var_shape)//"), }"

    do while (mod(len(str) + 11, 16) /= 0)
      str = str//" "
    enddo

    str = str//achar(10)

  end function dict_str

  function shape_str(var_shape) result(fin_str)
    integer(int32), intent(in)        :: var_shape(:)
    character(len=:), allocatable :: str, small_str, fin_str
    integer(int32)                    :: i, length, start, halt

    length = 14*size(var_shape)
    allocate (character(length) :: str)
    allocate (character(14)     :: small_str)
    str = " "

    do i = 1, size(var_shape)
      start = (i - 1)*length + 1
      halt = i*length + 1
      write (small_str, "(I13,A)") var_shape(i), ","
      str = trim(str)//adjustl(small_str)
    enddo

    fin_str = trim(str)
  end function shape_str
end module m_npy
