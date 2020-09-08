/*
 * Whiteflag Java Library
 */
package org.whiteflag.protocol.core;

/**
 * Whiteflag message segment class
 * 
 * </p> This is a class representing a segment of a Whiteflag message, such as a
 * the message header or the message body. A message segment contains a number of
 * message fields, depending on the part and the type of the message. This
 * class is used to construct different message types with a single interface.
 */
public class WfMessageSegment {

    /* PROPERTIES */

    /* Array of message segment fields */
    private WfMessageField[] fields;

    /* CONSTRUCTOR */

    /**
     * Constructs a message segment from a {@link WfMessageField} array
     * @param fieldArray an array of {@link WfMessageField}
     */
    public WfMessageSegment(WfMessageField[] fieldArray) {
        this.fields = fieldArray;
    }

    /* PUBLIC METHODS: internal interface */

    /**
     * Checks if all fields of this message segment contain valid data
     * @return TRUE if message segment contains valid data, esle FALSE
     */
    public Boolean isValid() {
        for (WfMessageField field : fields) {
            if (Boolean.TRUE.equals(!field.isValid())) return false;
        }
        return true;
    }

    /**
     * Serializes this message segment
     * @return a string with the serialized message segment
     * @throws WfCoreException if the message cannot be serialized
     */
    public String serialize() throws WfCoreException {
        int currentByte = fields[0].startByte;
        StringBuilder s = new StringBuilder();

        for (WfMessageField field : fields) {
            if (field.startByte != currentByte) {
                throw new WfCoreException("Invalid field order while serializing: did not expect field " + field.name + " at byte " + currentByte);
            }
            s.append(field.getValue());
            currentByte = field.endByte;
        }
        return s.toString();
    }

    /**
     * Encodes this message segment
     * @return a string with the serialized message segment
     * @throws WfCoreException if the message cannot be encoded
     */
    public WfBinaryString encode() throws WfCoreException {
        int currentByte = fields[0].startByte;
        WfBinaryString bin = new WfBinaryString();
        
        for (WfMessageField field : fields) {
            if (field.startByte != currentByte) {
                throw new WfCoreException("Invalid field order while encoding: did not expect field " + field.name + " at byte " + currentByte);
            }
            bin.add(field.encode());
            currentByte = field.endByte;
        }
        return bin;
    }

    /* PUBLIC METHODS */
    
    /**
     * Gets the field specified by name
     * @param name String with the name of the requested field
     * @return the requested {@link WfMessageField}, or NULL if it does not exist
     */
    public WfMessageField getField(String name) {
        for (WfMessageField field : fields) {
            if (name.equals(field.name)) return field;
        }
        return null;
    }

    /**
     * Gets the field specified by index
     * @param index integer with the index of the requested field
     * @return the requested {@link WfMessageField}, or NULL if it does not exist
     */
    public WfMessageField getField(int index) {
        if (index >= 0 && index < fields.length) {
            return fields[index];
        }
        return null;
    }

    /**
     * Gets the value of the field specified by name
     * @param name String with the name of the requested field
     * @return String with the field value, or NULL if field does not exist
     */
    public String getFieldValue(String name) {
        for (WfMessageField field : fields) {
            if (name.equals(field.name)) return field.getValue();
        }
        return null;
    }

    /**
     * Gets the value of the field specified by index
     * @param index integer with the index of the requested field
     * @return String with the field value, or NULL if it does not exist
     */
    public String l(int index) {
        if (index >= 0 && index < fields.length) {
            return fields[index].getValue();
        }
        return null;
    }

    /**
     * Adds additional fields to this message segment if constructing complex message bodies
     * @param segment {@link WfMessageSegment} to be added to the message segment
     * @return The updated message segment object
     */
    public WfMessageSegment add(WfMessageSegment segment) {
        int index = 0;
        WfMessageField[] newFields = segment.getFields();
        WfMessageField[] allFields = new WfMessageField[fields.length + newFields.length];
        for (WfMessageField field : fields) {
            allFields[index] = field;
            index++;
        }
        for (WfMessageField field : newFields) {
            allFields[index] = field;
            index++;
        }
        fields = allFields;
        return this;
    }

    /**
     * Sets the value of the specified field in the message segment
     * @param name String with the name of the field
     * @param data String with data to be set as the field value
     * @return TRUE if the data was valid and the field value is set, else FALSE
     */
    public Boolean setFieldValue(String name, String data) {
        for (WfMessageField field : fields) {
            if (name.equals(field.name)) return field.setValue(data);
        }
        return false;
    }

    /**
     * Sets the value of the field specified by its index in the message segment
     * @param index Integer with the name of the field
     * @param data String with data to be set as the field value
     * @return TRUE if the data was valid and the field value is set, else FALSE
     */
    public Boolean setFieldValue(int index, String data) {
        if (index >= 0 && index < fields.length) {
            return fields[index].setValue(data);
        }
        return false;
    }

    /**
     * Sets the value of all fields in the message segment with values from an array
     * @param data Array of strings with with data to be set as the field values
     * @return TRUE if the data was valid and all field values are set
     * @throws WfCoreException if the provided data is invalid
     */
    public Boolean setAllFieldValues(String[] data) throws WfCoreException {
        if (data.length != fields.length) {
            throw new WfCoreException("Message part has " + fields.length + " fields, but provided data for " + data.length + " fields");
        }
        for (int n = 0; n < fields.length; n++) {
            if (Boolean.TRUE.equals(!fields[n].setValue(data[n]))) {
                throw new WfCoreException("Invalid data provided for " + fields[n].name + " field: " + data[n] + " must match " + fields[n].pattern.toString());
            }
        }
        return true;
    }

    /**
     * Gets the number of fields in this message segment
     * @return integer with the numbver of fields
     */
    public int getNoFields() {
        return fields.length;
    }

    /* PROTECTED METHODS */

    /**
     * Gets all fields from this message segment
     * @return Array of {@link WfMessageField}
     */
    protected WfMessageField[] getFields() {
        return fields;
    }
}
