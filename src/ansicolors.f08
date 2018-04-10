! w-ansi-colors
! (c) 2017-2018 Brendyn Sonntag
! Licensed under Apache 2.0
module ansicolors
        !---------------------------------------------------------------------------------------------------------------------------
        ! Definition - "Globals"
        !---------------------------------------------------------------------------------------------------------------------------

        character(len=:), allocatable :: c_reset, c_red, c_green, c_yellow, c_blue, c_magenta, c_cyan, c_bright_white

        !---------------------------------------------------------------------------------------------------------------------------
        ! Subroutines
        !---------------------------------------------------------------------------------------------------------------------------

        contains

        subroutine platformcolors()
                ! [Windows]
                ! Only windows 10 Creators Update supports colors in CMD.
                ! TODO: Detect Windows 10, and allow colors.
                ! Probable implementation: Execute `ver`, and read the NT version string.
                ! [Linux]
                ! It's probably safe to assume all POSIX compatible terminals allow ANSI escape codes.
                logical :: isWindows
                inquire (file="C:/", exist=isWindows)
                call usecolors(.not. isWindows)
        end subroutine platformcolors

        subroutine usecolors(toggle)
                ! If colors were previously on and now to be turned off, be sure to print c_reset before calling this.
                logical, intent(in) :: toggle
                character :: esc
                esc = achar(27)

                if (allocated(c_reset)) deallocate(c_reset)
                if (allocated(c_red)) deallocate(c_red)
                if (allocated(c_green)) deallocate(c_green)
                if (allocated(c_yellow)) deallocate(c_yellow)
                if (allocated(c_blue)) deallocate(c_blue)
                if (allocated(c_magenta)) deallocate(c_magenta)
                if (allocated(c_cyan)) deallocate(c_cyan)
                if (allocated(c_bright_white)) deallocate(c_bright_white)

                if (toggle) then
                        c_reset = esc//"[0m"
                        c_red = esc//"[91m"
                        c_green = esc//"[92m"
                        c_yellow = esc//"[93m"
                        c_blue = esc//"[94m"
                        c_magenta = esc//"[95m"
                        c_cyan = esc//"[96m"
                        c_bright_white = esc//"[97m"
                else
                        c_reset = ""
                        c_red = ""
                        c_green = ""
                        c_yellow = ""
                        c_blue = ""
                        c_magenta = ""
                        c_cyan = ""
                        c_bright_white = ""
                end if
        end subroutine usecolors


end module ansicolors
