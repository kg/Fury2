Attribute VB_Name = "mdlPSP"
'
'   PSP File Loader v1.0
'   Versions supported: 5.0
'

Public Enum PSPBlockID
    PSP_IMAGE_BLOCK    ' General Image Attributes Block (main)
    PSP_CREATOR_BLOCK  ' Creator Data Block (main)
    PSP_COLOR_BLOCK        ' Color Palette Block (main and sub)
    PSP_LAYER_START_BLOCK  ' Layer Bank Block (main)
    PSP_LAYER_BLOCK        ' Layer Block (sub)
    PSP_CHANNEL_BLOCK  ' Channel Block (sub)
    PSP_SELECTION_BLOCK    ' Selection Block (main)
    PSP_ALPHA_BANK_BLOCK   ' Alpha Bank Block (main)
    PSP_ALPHA_CHANNEL_BLOCK ' Alpha Channel Block (sub)
    PSP_COMPOSITE_IMAGE_BLOCK ' Composite Image Block (sub)
    PSP_EXTENDED_DATA_BLOCK ' Extended Data Block (main)
    PSP_TUBE_BLOCK     ' Picture Tube Data Block (main)
    PSP_ADJUSTMENT_EXTENSION_BLOCK ' Adjustment Layer Block (sub)
    PSP_VECTOR_EXTENSION_BLOCK ' Vector Layer Block (sub)
    PSP_SHAPE_BLOCK        ' Vector Shape Block (sub)
    PSP_PAINTSTYLE_BLOCK   ' Paint Style Block (sub)
    PSP_COMPOSITE_IMAGE_BANK_BLOCK ' Composite Image Bank (main)
    PSP_COMPOSITE_ATTRIBUTES_BLOCK ' Composite Image Attr. (sub)
    PSP_JPEG_BLOCK     ' JPEG Image Block (sub)
    PSP_LINESTYLE_BLOCK    ' Line Style Block (sub)
    PSP_TABLE_BANK_BLOCK   ' Table Bank Block (main)
    PSP_TABLE_BLOCK        ' Table Block (sub)
    PSP_PAPER_BLOCK        ' Vector Table Paper Block (sub)
    PSP_PATTERN_BLOCK  ' Vector Table Pattern Block (sub)
End Enum

Public Enum PSPDIBType
    PSP_DIB_IMAGE = 0      ' Layer color bitmap
    PSP_DIB_TRANS_MASK     ' Layer transparency mask bitmap
    PSP_DIB_USER_MASK      ' Layer user mask bitmap
    PSP_DIB_SELECTION      ' Selection mask bitmap
    PSP_DIB_ALPHA_MASK     ' Alpha channel mask bitmap
    PSP_DIB_THUMBNAIL      ' Thumbnail bitmap
    PSP_DIB_THUMBNAIL_TRANS_MASK   ' Thumbnail transparency mask
    PSP_DIB_ADJUSTMENT_LAYER   ' Adjustment layer bitmap
    PSP_DIB_COMPOSITE      ' Composite image bitmap
    PSP_DIB_COMPOSITE_TRANS_MASK   ' Composite image transparency
    PSP_DIB_PAPER          ' Paper bitmap
    PSP_DIB_PATTERN            ' Pattern bitmap
    PSP_DIB_PATTERN_TRANS_MASK ' Pattern transparency mask
End Enum

Public Enum PSPCompression
    PSP_COMP_NONE = 0      ' No compression
    PSP_COMP_RLE           ' RLE compression
    PSP_COMP_LZ77          ' LZ77 compression
    PSP_COMP_JPEG           ' JPEG compression (only used by
                                        ' thumbnail and composite image)
End Enum

Public Enum PSPGraphicContents
' Layer types
keGCRasterLayers = &H1             ' At least one raster layer
keGCVectorLayers = &H2             ' At least one vector layer
keGCAdjustmentLayers = &H4         ' At least one adjust. layer
' Additional attributes
keGCThumbnail = &H1000000               ' Has a thumbnail
keGCThumbnailTransparency = &H2000000   ' Thumbnail transp.
keGCComposite = &H4000000               ' Has a composite image
keGCCompositeTransparency = &H8000000   ' Composite transp.
keGCFlatImage = &H10000000              ' Just a background
keGCSelection = &H20000000              ' Has a selection
keGCFloatingSelectionLayer = &H40000000 ' Has float. selection
keGCAlphaChannels = &H80000000          ' Has alpha channel(s)
End Enum

Public Type PSPBlockHeader
    Header As String * 4
    Type As Integer
    Length As Long
End Type
