program submarine
    use depth_gauge
    implicit none

    integer :: i

    call read_data("input.txt")

    ! until i is the depth array length
    do i = 1, size(depths)
        call get_direction(i)
    end do

    print *, 'Depth increase count: ', incr_count
    print *, 'Depth decrease count:', decr_count

    call reset_guage()
    ! do i = 1, size(depths)
        ! call window_counter(i)
    ! end do

    ! watch your memory
    deallocate(depths)
end program submarine