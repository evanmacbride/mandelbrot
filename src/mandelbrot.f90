! Adapted from
! https://rosettacode.org/wiki/Mandelbrot_set#Fortran

program mandelbrot
  implicit none
  integer  , parameter :: rk       = selected_real_kind (9, 99)
  integer  , parameter :: n_max    =  100
  real (rk), parameter :: x_centre = -0.5_rk
  real (rk), parameter :: y_centre =  0.0_rk
  real (rk), parameter :: width    =  4.0_rk
  real (rk), parameter :: height   =  3.0_rk
  real (rk) :: dx_di
  real (rk) :: dy_dj
  real (rk) :: x_offset
  real (rk) :: y_offset
  integer   :: i_max
  integer   :: j_max
  integer, dimension (:, :), allocatable :: image
  integer   :: i
  integer   :: j
  integer   :: n
  INTEGER   :: istat
  real (rk) :: x
  real (rk) :: y
  real (rk) :: x_0
  real (rk) :: y_0
  real (rk) :: x_sqr
  real (rk) :: y_sqr
  real(rk) :: start_time, stop_time
  character(len=12) :: fmt
  character(len=12) :: grid_size, out_file
  character(len=80) :: msg

  ! Set i_max to command line argument, if present
  IF (COMMAND_ARGUMENT_COUNT() > 0) THEN
    CALL GET_COMMAND_ARGUMENT(1,grid_size)
    READ(grid_size, *, IOSTAT=istat, IOMSG=msg) i_max
    IF (istat /= 0) THEN
      WRITE(*, *) msg
      WRITE(*, *) "Usage: ./mandelbrot grid_size filename"
      CALL EXIT(istat)
    END IF
  ELSE
    i_max = 256
  END IF

  !i_max    = 256
  j_max    = i_max
  dx_di    =   width / i_max
  dy_dj    = -height / j_max
  x_offset = x_centre - 0.5_rk * (i_max + 1) * dx_di
  y_offset = y_centre - 0.5_rk * (j_max + 1) * dy_dj
  ALLOCATE(image(i_max, j_max))

  ! Get the format string for the output file
  write(fmt,'(G0)') i_max
  fmt = '(' // TRIM(fmt) // 'I5)'

  call cpu_time(start_time)
  do j = 1, j_max
    y_0 = y_offset + dy_dj * j
    do i = 1, i_max
      x_0 = x_offset + dx_di * i
      x = 0.0_rk
      y = 0.0_rk
      n = 0
      do
        x_sqr = x ** 2
        y_sqr = y ** 2
        if (x_sqr + y_sqr > 4.0_rk) then
          image(i, j) = INT(255._rk * REAL(n, rk) / REAL(n_max,rk))
          exit
        end if
        if (n == n_max) then
          image (i, j) = 0
          exit
        end if
        y = y_0 + 2.0_rk * x * y
        x = x_0 + x_sqr - y_sqr
        n = n + 1
      end do
    end do
  end do
  call cpu_time(stop_time)
  write(*,'(a,f10.3,a)')  ' completed in ', stop_time-start_time, ' seconds'
  IF (COMMAND_ARGUMENT_COUNT() > 1) THEN
    CALL GET_COMMAND_ARGUMENT(2, out_file)
    open  (10, file = out_file, action='WRITE', status='REPLACE' )
    write (10, fmt) ((image(i, j), i = 1, i_max), j = 1, j_max)
    close (10)
  END IF
end program mandelbrot
