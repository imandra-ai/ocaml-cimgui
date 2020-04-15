

module Make(Types : Cstubs_structs.TYPE) = struct
  open! Types
  open Ctypes_static

  module InitInfo = struct
    type t = [`Vulkan_init_info] abstract
    let t : t typ =
      abstract ~name:"ImGui_ImplVulkan_InitInfo" ~alignment:32 ~size:8
    (* TODO: fields? but we'd have to bind the whole Vulkan API to get sizeof
    let vk_instance = field t "VkInstance" 

        *)

  end
end
