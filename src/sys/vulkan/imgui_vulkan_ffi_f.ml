open Ctypes

module Types = Imgui_vulkan_types_f.Make(Imgui_vulkan_generated_types)

module Make(F : Cstubs.FOREIGN) = struct
  open! Types
  open! F

  let init =
    foreign "ImGui_ImplVulkan_Init"
      (ptr_opt InitInfo.t @-> returning bool)

  let shutdown =
    foreign "ImGui_ImplVulkan_Shutdown"
      (void @-> returning void)

  let new_frame =
    foreign "ImGui_ImplVulkan_NewFrame"
      (void @-> returning void)

  (* TODO
IMGUI_IMPL_API void     ImGui_ImplVulkan_RenderDrawData(ImDrawData* draw_data, VkCommandBuffer command_buffer);
IMGUI_IMPL_API bool     ImGui_ImplVulkan_CreateFontsTexture(VkCommandBuffer command_buffer);
IMGUI_IMPL_API void     ImGui_ImplVulkan_DestroyFontUploadObjects();
IMGUI_IMPL_API void     ImGui_ImplVulkan_SetMinImageCount(uint32_t min_image_count); // To override MinImageCount after initialization (e.g. if swap chain is recreated)
     *)
end
