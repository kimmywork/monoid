open Echo_service

module Echo_impl : Echo_stub = struct
  open Echo_message

  let invoke { message } = { message = "Echo " ^ message }
end
