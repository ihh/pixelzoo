# Copyright (C) 2009 The Android Open Source Project
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

MY_LOCAL_PATH := $(call my-dir)



# include $(call all-subdir-makefiles)
include $(MY_LOCAL_PATH)/xml2/Android.mk
include $(MY_LOCAL_PATH)/hello-jni/Android.mk

include Settings.mk # for SDL
include $(MY_LOCAL_PATH)/SDL/Android.mk
# include $(MY_LOCAL_PATH)/sdl-1.3-progress/Android.mk

LOCAL_PATH := $(MY_LOCAL_PATH)

include $(CLEAR_VARS)

# LOCAL_MODULE    := xmltest
# LOCAL_SRC_FILES := xmltest.c

LOCAL_MODULE := pixelzoo
LOCAL_SRC_FILES := $(wildcard ../../src/*.c)

LOCAL_LDLIBS := -llog
LOCAL_SHARED_LIBRARIES := xml2
LOCAL_C_INCLUDES := xml2/include
LOCAL_CFLAGS = -std=c99

include $(BUILD_SHARED_LIBRARY)
