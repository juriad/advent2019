module types
    type LineType
        integer :: startx, starty, endx, endy
        integer :: steps_before
        logical :: vertical
    end type LineType

    type CrossingType
        integer :: x, y, steps1, steps2
    end type CrossingType

end module types

program prg
    use types
    implicit none

    character(len = 256) :: arg

    integer :: file_size
    type(LineType), dimension(:), allocatable :: lines1, lines2
    type(CrossingType), dimension(:), allocatable :: crossings

    integer m1, m2

    call getarg(1, arg)
    inquire(file = arg, size = file_size)
    print *, file_size

    open(file = arg, unit = 9)
    lines1 = read_lines(file_size)
    lines2 = read_lines(file_size)
    close(unit = 9)

    print *, size(lines1), size(lines2)

    crossings = find_crossings(lines1, lines2)
    print *, size(crossings)

    m1 = find_min1(crossings)
    print *, m1

    m2 = find_min2(crossings)
    print *, m2

    deallocate(lines1)
    deallocate(lines2)
    deallocate(crossings)

contains

    function read_lines(size)
        implicit none

        integer :: size
        type(LineType), dimension(:), allocatable :: read_lines

        character(len = file_size) :: buffer
        character :: c

        integer :: count, n
        integer :: startx, starty, dx, dy, l, steps_before

        read(9, '(A)', end = 99) buffer
        99 continue

        print *, "read"

        count = 1
        do n = 1, len_trim(buffer)
            c = buffer (n:n)
            if (c == ',') then
                count = count + 1
            end if
        end do

        allocate(read_lines(count))

        count = 1
        startx = 0
        starty = 0
        l = 0
        steps_before = 0

        do n = 1, len_trim(buffer) + 1
            c = buffer(n:n)
            ! print *, c

            select case(c)
            case('L')
                dx = -1
                dy = 0
            case('R')
                dx = 1
                dy = 0
            case('U')
                dx = 0
                dy = 1
            case('D')
                dx = 0
                dy = -1
            case('0':'9')
                l = l * 10 + ichar(c) - 48
            case default
                read_lines(count)%startx = startx
                read_lines(count)%starty = starty

                startx = startx + dx * l
                starty = starty + dy * l

                read_lines(count)%endx = startx
                read_lines(count)%endy = starty

                read_lines(count)%steps_before = steps_before

                read_lines(count)%vertical = read_lines(count)%startx == startx

                ! print *, l, dx, dy

                steps_before = steps_before + l
                l = 0
                count = count + 1
            end select
        end do

        print *, size, count, ": ", read_lines(1), "; ", read_lines(count - 2)

    end function read_lines

    function find_crossings(lines1, lines2)
        type(LineType), dimension(:), allocatable :: lines1
        type(LineType), dimension(:), allocatable :: lines2

        type(LineType) :: l1, l2

        type(CrossingType), dimension(:), allocatable :: find_crossings

        integer :: i, j, count

        count = 0
        do i = 1, size(lines1)
            l1 = lines1(i)
            do j = 1, size(lines2)
                l2 = lines2(j)

                if (l1%vertical) then
                    if (l2%vertical) then
                        ! nothing
                    else
                        if (l1%startx >= min(l2%startx, l2%endx) .and. l1%startx <= max(l2%endx, l2%startx) &
                                .and. l2%starty >= min(l1%starty, l1%endy) .and. l2%starty <= max(l1%endy, l1%starty)) then
                            count = count + 1
                        end if
                    end if
                else
                    if (l2%vertical) then
                        if (l2%startx >= min(l1%startx, l1%endx) .and. l2%startx <= max(l1%endx, l1%startx) &
                                .and. l1%starty >= min(l2%starty, l2%endy) .and. l1%starty <= max(l2%endy, l2%starty)) then
                            count = count + 1
                        end if
                    else
                        ! nothing
                    end if
                end if
            end do
        end do

        allocate(find_crossings(count))

        count = 0
        do i = 1, size(lines1)
            l1 = lines1(i)
            do j = 1, size(lines2)
                l2 = lines2(j)

                ! print *, l1, l2

                if (l1%vertical) then
                    if (l2%vertical) then
                        ! nothing
                    else
                        if (l1%startx >= min(l2%startx, l2%endx) .and. l1%startx <= max(l2%endx, l2%startx) &
                                .and. l2%starty >= min(l1%starty, l1%endy) .and. l2%starty <= max(l1%endy, l1%starty)) then
                            count = count + 1

                            find_crossings(count)%x = l1%startx
                            find_crossings(count)%y = l2%starty

                            find_crossings(count)%steps1 = l1%steps_before + abs(find_crossings(count)%y - l1%starty)
                            find_crossings(count)%steps2 = l2%steps_before + abs(find_crossings(count)%x - l2%startx)

                            print *, "VH", find_crossings(count)
                        end if
                    end if
                else
                    if (l2%vertical) then
                        if (l2%startx >= min(l1%startx, l1%endx) .and. l2%startx <= max(l1%endx, l1%startx) &
                                .and. l1%starty >= min(l2%starty, l2%endy) .and. l1%starty <= max(l2%endy, l2%starty)) then
                            count = count + 1

                            find_crossings(count)%x = l2%startx
                            find_crossings(count)%y = l1%starty

                            find_crossings(count)%steps1 = l1%steps_before + abs(find_crossings(count)%x - l1%startx)
                            find_crossings(count)%steps2 = l2%steps_before + abs(find_crossings(count)%y - l2%starty)

                            print *, "HV", find_crossings(count)
                        end if
                    else
                        ! nothing
                    end if
                end if
            end do
        end do

        ! print *, count, ": ", find_crossings

    end function find_crossings

    function find_min1(crossings)
        type(CrossingType), dimension(:), allocatable :: crossings

        integer :: find_min1, n, m

        find_min1 = -1
        do n = 1, size(crossings)
            m = abs(crossings(n)%x) + abs(crossings(n)%y)
            if (m < find_min1 .or. find_min1 == -1) then
                if (m > 0) then
                    find_min1 = m
                end if
            end if
        end do

    end function find_min1

    function find_min2(crossings)
        type(CrossingType), dimension(:), allocatable :: crossings

        integer :: find_min2, n, m

        find_min2 = -1
        do n = 1, size(crossings)
            m = crossings(n)%steps1 + crossings(n)%steps2
            if (m < find_min2 .or. find_min2 == -1) then
                if (m > 0) then
                    find_min2 = m
                end if
            end if
        end do

    end function find_min2

end program prg
