program analysis
  integer, parameter :: target_id = 538303
  integer, allocatable :: dates(:), ids(:), u_dates(:), u_ids(:)
  real, allocatable :: A(:,:), b(:,:)
  real :: p
  integer :: i
  character(len=200) :: fn

  call get_command_argument(1, fn)
  if (len_trim(fn)==0) then
     write(0,*) 'Expected input filename as first argument.'
     error stop 1
  end if

  call load_data(fn, ids, dates, b)
  call make_A(ids, dates, u_ids, u_dates, A)

  do i = 1, size(dates)
     if (ids(i) == target_id) then
        print '(I0, A, F12.2, A, I0)', dates(i), char(9), exp(b(i,1)), char(9), 1
     end if
  end do
  
  call fit(A, b)
  p = b(size(u_dates) + findloc(u_ids, target_id, dim=1),1)
  do i = 1, size(u_dates)
     print '(I0, A, F12.2, A, I0)', u_dates(i), char(9), exp(b(i,1)+p), char(9), 0
  end do

contains
  subroutine fit(A, b)
    real, intent(inout) :: A(:,:), b(:,:)
    real, allocatable :: work(:)
    integer :: m, n, w_size, info

    m = size(A,1); n = size(A,2)
    allocate(work(1))
    call sgels('N', m, n, 1, A, m, b, m, work, -1, info)
    w_size = int(work(1))
    deallocate(work)
    allocate(work(w_size))
    call sgels('N', m, n, 1, A, m, b, m, work, w_size, info)
    if (info /= 0) then
       write(0,*) 'DGELS failed.'
       error stop
    endif
  end subroutine fit

  subroutine make_A(ids, dates, u_ids, u_dates, A)
    integer, intent(in) :: ids(:), dates(:)
    integer, allocatable :: u_ids(:), u_dates(:)
    real, allocatable :: A(:,:)
    integer :: i, j, k

    u_dates = unique(dates)
    u_ids   = unique(ids)
    allocate(A(size(ids), size(u_dates) + size(u_ids)))

    A = 0
    do i = 1, size(ids)
       j = findloc(u_dates, dates(i), dim=1)
       k = findloc(u_ids, ids(i), dim=1)
       A(i, j) = 1
       A(i, size(u_dates) + k) = 1
    end do
  end subroutine make_A
    
  subroutine load_data(fn, ids, dates, prices)
    character(len=200), intent(in) :: fn
    integer, allocatable, intent(out) :: dates(:), ids(:)
    real, allocatable, intent(out) :: prices(:,:)
    integer :: fd, ios, i1, i2, i, lines
    real :: r6
    character :: c3, c4, c5
    character(len=100) :: line

    open(newunit=fd, file=fn, status='old', action='read')
    lines = count_lines(fd)
    allocate(dates(lines), ids(lines), prices(lines,1))

    i = 1
    do
       read(fd, *, iostat=ios) i1, i2, c3, c4, c5, r6
       if (ios /= 0) exit
       ids(i)      = i1
       dates(i)    = i2 / 100
       prices(i,1) = log(r6)
       i = i + 1
    end do
    close(fd)
  end subroutine load_data

  function count_lines(fd) result(count)
    integer, intent(in) :: fd
    integer :: count, ios
    character(len=100) :: line

    count = 0
    rewind(fd)
    do
       read(fd, '(A)', iostat=ios) line
       if (ios /= 0) exit
       count = count + 1
    end do
    rewind(fd)
  end function count_lines
    
  pure function unique(vec) result(unq)
    integer, intent(in) :: vec(:)
    integer, allocatable :: unq(:)
    integer :: i
      
    allocate(unq(maxval(vec)+1))
    unq = -1
    do i = 1, size(vec)
       unq(vec(i)) = vec(i) 
    end do
    unq = pack(unq, unq>-1)
  end function unique
end program analysis
