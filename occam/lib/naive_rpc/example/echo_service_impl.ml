open Echo_service

module Echo_impl : Echo_stub = struct
  open Echo_message

  let invoke { message } = { message = "Echo from server: " ^ message }
end

module Second_impl : Second_stub = struct
  let invoke Second_request.{ message; age } =
    Echo_message.
      {
        message =
          "Second from server: " ^ message ^ ", age: " ^ string_of_int age;
      }
end
