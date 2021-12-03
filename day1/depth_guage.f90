module depth_gauge
    implicit none

    private
    public get_direction, read_data, depths, incr_count, decr_count, window_counter, reset_guage

    integer, allocatable :: depths(:)
    integer :: incr_count
    integer :: decr_count

    contains

    ! returns the submarines current direction
    subroutine get_direction(index)
        integer, intent(in) :: index
        integer :: depth
        integer :: old_depth

        depth = depths(index)

        if (index > 1) then
            old_depth = depths(index - 1)
        
            if (depth < old_depth) then
                print *, depth, 'decreased'
                decr_count = decr_count + 1
            else if (depth > old_depth) then
                print *, depth, 'increased'
                incr_count = incr_count + 1
            else
                print *, depth, 'no change'
            end if
        else
            print *, depth, 'no change (starting value)'
        end if

    end subroutine get_direction

    !Welcome to hell :)))
    subroutine read_data(file_name)
        character (len=*), intent(in) :: file_name
        integer :: file_status
        integer :: i

        open(unit=1, file=file_name, iostat=file_status, action='read')
        if (file_status /= 0) stop "open file error"

        allocate(depths(2000)) ! Hard coding file length because i am very smart
        do i = 1, 2000
            read(1, *, iostat=file_status) depths(i)
        end do
        if ( file_status /= 0 ) stop "error reading file :("
        

        close(1)
    end subroutine read_data

    ! reset variables what more can i say really
    subroutine reset_guage()
        incr_count = 0
        decr_count = 0
    end subroutine reset_guage

    ! print and tally data directly from the input file
    subroutine raw_data_counter()

    end subroutine raw_data_counter

    ! print and tally data using groups of 3
    subroutine window_counter(index)
        integer, intent(in) :: index
        call get_direction(index)

    end subroutine window_counter
end module depth_gauge