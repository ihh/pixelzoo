//
//  Base64.m
//  PixelZoo
//
//  Created by Ian Holmes on 10/14/13.
//  Copyright (c) 2013 Holmesian Software. All rights reserved.
//

#import "Base64.h"

@implementation Base64

static char *alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

+(NSString *)encode:(NSData *)plainText {
    int encodedLength = (4 * (([plainText length] / 3) + (1 - (3 - ([plainText length] % 3)) / 3))) + 1;
    char *outputBuffer = malloc(encodedLength);
    char *inputBuffer = (char *)[plainText bytes];
    
    NSInteger i;
    NSInteger j = 0;
    int remain;
    
    for(i = 0; i < [plainText length]; i += 3) {
        remain = [plainText length] - i;
        
        outputBuffer[j++] = alphabet[(inputBuffer[i] & 0xFC) >> 2];
        outputBuffer[j++] = alphabet[((inputBuffer[i] & 0x03) << 4) |
                                     ((remain > 1) ? ((inputBuffer[i + 1] & 0xF0) >> 4): 0)];
        
        if(remain > 1)
            outputBuffer[j++] = alphabet[((inputBuffer[i + 1] & 0x0F) << 2)
                                         | ((remain > 2) ? ((inputBuffer[i + 2] & 0xC0) >> 6) : 0)];
        else
            outputBuffer[j++] = '=';
        
        if(remain > 2)
            outputBuffer[j++] = alphabet[inputBuffer[i + 2] & 0x3F];
        else
            outputBuffer[j++] = '=';
    }
    
    outputBuffer[j] = 0;
    
    NSString *result = [NSString stringWithUTF8String:outputBuffer];
    free(outputBuffer);
    
    return result;
}


+ (void) addBasicAuthHeader:(NSMutableURLRequest*)request forUser:(NSString*)user withPass:(NSString*)pass {

    // create a plaintext string in the format username:password
    NSMutableString *loginString = (NSMutableString*)[@"" stringByAppendingFormat:@"%@:%@", user, pass];

    // employ the Base64 encoding above to encode the authentication tokens
    NSString *encodedLoginData = [Base64 encode:[loginString dataUsingEncoding:NSUTF8StringEncoding]];

    // create the contents of the header
    NSString *authHeader = [@"Basic " stringByAppendingFormat:@"%@", encodedLoginData];

    // add the header to the request.  Here's the $$$!!!
    [request addValue:authHeader forHTTPHeaderField:@"Authorization"];
}

@end
