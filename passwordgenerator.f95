program PasswordGenerator
  implicit none

  character(128) :: lowercase_characters
  character(128) :: uppercase_characters
  character(128) :: numbers
  character(128) :: special_symbols

  character(512) :: character_set
  character(128) :: password

  integer :: length, i
  logical :: use_lowercase, use_uppercase, use_numbers, use_special
  character(128) :: arg

  integer :: random_index
  real :: random_index_real
  character(1) :: selected_char

  ! Check for command-line arguments
  if (command_argument_count() < 3) then
    print *, "Usage: ./PasswordGenerator -L <length> [-l] [-u] [-n] [-s]"
    stop
  end if

  ! Initialize variables
  lowercase_characters = 'abcdefghijklmnopqrstuvwxyz'
  uppercase_characters = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
  numbers = '0123456789'
  special_symbols = '!@#$%^&*()_+[]{}|;:,.<>?'

  character_set = ''
  password = ''
  length = 0
  arg = ''
  
  use_lowercase = .false.
  use_uppercase = .false.
  use_numbers = .false.
  use_special = .false.

  ! Parse command-line arguments
  i = 1
  do while (i <= command_argument_count())
    call get_command_argument(i, value=arg)  ! Get the argument as a string
    select case (arg)
    case ('-L')
      if (i < command_argument_count()) then
        i = i + 1
        call get_command_argument(i, value=arg)  ! Get the argument after -L
        read(arg, *) length  ! Convert the string to an integer
      else
        print *, "Missing value for -L option."
        stop
      end if
    case ('-l')
      use_lowercase = .true.
    case ('-u')
      use_uppercase = .true.
    case ('-n')
      use_numbers = .true.
    case ('-s')
      use_special = .true.
    end select
    i = i + 1
  end do


  ! Set character set based on command-line arguments
  if (use_lowercase) then
    character_set = trim(character_set) // trim(lowercase_characters)
  end if

  if (use_numbers) then
    character_set = trim(character_set) // trim(numbers)
  end if

  if (use_uppercase) then
    character_set = trim(character_set) // trim(uppercase_characters)
  end if

  if (use_special) then
    character_set = trim(character_set) // trim(special_symbols)
  end if

  ! Check if the character set is not empty and the password length is greater than 0
  if (len_trim(character_set) <= 0) then
    print *, "Character set is empty."
  else if (length <= 0) then
    print *, "Length is not set."
  else
    do i = 1, length
      ! Generate a random index within the bounds of the character_set
        random_index_real = real(random_index)
      call random_number(random_index_real)
      random_index = 1 + nint(random_index_real * len_trim(character_set))

      ! Select a character from the character set
      selected_char = character_set(random_index:random_index)

      ! Append the selected character to the password
      password = trim(password) // trim(selected_char)
    end do

    ! Print the generated password
    print *, "Generated password: ", password

  end if

end program PasswordGenerator
