# ##### BEGIN GPL LICENSE BLOCK #####
#
#  This program is free software; you can redistribute it and/or
#  modify it under the terms of the GNU General Public License
#  as published by the Free Software Foundation; either version 2
#  of the License, or (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software Foundation,
#  Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
#
# ##### END GPL LICENSE BLOCK #####

# Author: Adhi Hargo (cadmus.sw@gmail.com)

import bpy
import os
import subprocess
import tempfile
from bpy.types import Operator, Menu
from bpy.props import EnumProperty, IntProperty, StringProperty

bl_info = {
    "name": "ADH Fluid Concept",
    "author": "Adhi Hargo",
    "version": (1, 0, 0),
    "blender": (2, 68, 0),
    "location": "View3D > Tools > ADH Fluid Concept",
    "description": "In-house production tools.",
    "warning": "",
    "wiki_url": "https://github.com/adhihargo/fluid_concept",
    "tracker_url": "https://github.com/adhihargo/fluid_concept/issues",
    "category": "Sequencer"}

def get_scene_enums(self, context):
    enums = [(s.name, s.name, 'Scene "%s"' % s.name) for s in bpy.data.scenes
             if s != context.scene]
    if not enums:
        enums.append((context.scene.name, "(no other scene)",
                      "There is no other scene to choose from."))
    return enums

def get_topmost_channel(sequences, frame_min, frame_max):
    frame_range = range(frame_min, frame_max)
    sequences_in_range = list(filter(lambda s:\
                                         s.frame_final_start in frame_range or\
                                         s.frame_final_end in frame_range,
                                     sequences))
    return max(map(lambda s: s.channel, sequences_in_range))\
        if sequences_in_range else 1

def render_image(context, filepath, scale = 100, scene = None):
    # Save, render...
    if context.space_data.type == 'VIEW_3D':
        prev_view_persp = context.space_data.region_3d.view_perspective
    prev_scene = context.screen.scene
    if scene:
        context.screen.scene = scene

    render_settings = context.scene.render
    prev_filepath = render_settings.filepath
    prev_fileformat = render_settings.image_settings.file_format
    prev_render_ratio = render_settings.resolution_percentage
    render_settings.filepath = filepath
    render_settings.image_settings.file_format = 'PNG'
    render_settings.resolution_percentage = scale
    bpy.ops.render.render(animation = False, write_still = True)

    # ... restore.
    render_settings.filepath = prev_filepath
    render_settings.image_settings.file_format = prev_fileformat
    render_settings.resolution_percentage = prev_render_ratio

    context.screen.scene = prev_scene
    if context.space_data.type == 'VIEW_3D':
        context.space_data.region_3d.view_perspective = prev_view_persp

class VIEW3D_OT_adh_background_from_other_scene(Operator):
    bl_idname = 'view3d.adh_background_from_other_scene'
    bl_label = 'Add Background from Other Scene'
    bl_options = {'REGISTER', 'UNDO'}

    PREVIEW_DATA_NAME = 'sequencer_preview'

    scene_name = EnumProperty(items = get_scene_enums)
    scale = IntProperty(min = 0, max = 100,
                        default = 100, subtype = 'PERCENTAGE')
    invoked = False

    @classmethod
    def poll(self, context):
        return context.space_data.type == 'VIEW_3D'

    def draw(self, context):
        layout = self.layout
        if self.invoked:
            return
        
        row = layout.row()
        row.label("Scene:")
        row.prop(self, 'scene_name', text="")

        row = layout.row()
        row.label("Resolution Percentage:")
        row.prop(self, 'scale', text="")

    def execute(self, context):
        if self.scene_name == context.scene.name:
            return {'CANCELLED'}
        space = context.space_data
        space.show_background_images = True

        temp_dir = context.user_preferences.filepaths.temporary_directory
        if not temp_dir:
            temp_dir = tempfile.gettempdir()
        
        bkg_preview = None
        prv_filepath = os.path.join(temp_dir, self.PREVIEW_DATA_NAME + '.png')
        for bkg_image in space.background_images:
            if bkg_image.image.name.startswith(self.PREVIEW_DATA_NAME):
                bkg_preview = bkg_image
                continue
            bkg_image.show_background_image = False

        scene = bpy.data.scenes[self.scene_name]
        render_image(context, prv_filepath, scale = self.scale, scene = scene)

        if not bkg_preview:
            bkg_preview = space.background_images.new()
            bkg_image = bpy.data.images.load(prv_filepath)
            bkg_preview.image = bkg_image
        else:
            bkg_preview.image.filepath = prv_filepath
            bkg_preview.image.reload()

        return {'FINISHED'}

    def invoke(self, context, event):
        retval = context.window_manager.invoke_props_dialog(self)
        self.invoked = True
        return retval

class SEQUENCER_OT_adh_add_annotation_image_strip(Operator):
    bl_idname = 'sequencer.adh_add_annotation_image_strip'
    bl_label = 'Add Annotation Image Strip'
    bl_options = {'REGISTER', 'UNDO'}

    filepath = StringProperty(subtype = 'FILE_PATH')
    invoked = False

    @classmethod
    def poll(self, context):
        return context.space_data.type == 'SEQUENCE_EDITOR'

    def draw(self, context):
        if self.invoked:
            return

        self.layout.prop(self, 'filepath', text="File Path")

    def execute(self, context):
        image_editor = context.user_preferences.filepaths.image_editor
        self.filepath = bpy.path.abspath(self.filepath)

        if not image_editor:
            self.report({'ERROR'}, 'No image editor set in User Preferences.')
            return {'CANCELLED'}
        render_image(context, self.filepath)
        if not os.path.exists(self.filepath):
            self.report({'ERROR'}, "Render output file doesn't exist.")
            return {'CANCELLED'}

        scene = context.scene
        strip_duration = 50
        topmost_channel = 1
        scene.sequence_editor_create()
        sequences = scene.sequence_editor.sequences
        if scene.sequence_editor:
            frame_current = scene.frame_current
            topmost_channel = get_topmost_channel(
                sequences, frame_current, frame_current + strip_duration)
        image_strip = sequences.new_image(
            os.path.basename(self.filepath), self.filepath,
            topmost_channel + 1, scene.frame_current)
        image_strip.blend_type = 'ALPHA_OVER'
        image_strip.frame_final_duration = strip_duration
        image_strip.mute = True # Unmute manually after edit = reload image
        
        proc = subprocess.Popen([image_editor, self.filepath])
        return {'FINISHED'}

    def invoke(self, context, event):
        self.filepath = '//frame_%04d.png' % (context.scene.frame_current)

        retval = context.window_manager.invoke_props_dialog(self)
        self.invoked = True
        return retval

def draw_sequencer_menu(self, context):
    pass

def register():
    bpy.types.SEQUENCER_HT_header.prepend(draw_sequencer_menu)
    bpy.utils.register_module(__name__)

def unregister():
    bpy.types.SEQUENCER_HT_header.remove(draw_sequencer_menu)
    bpy.utils.unregister_module(__name__)

if __name__ == "__main__":
    register()
